#!/usr/bin/env bash

set -euo pipefail

project_root="${1:-$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)}"
c_src_dir="${project_root}/apps/erldist_filter/c_src"
restore_build="${SANITIZERS_RESTORE_BUILD:-1}"
stress_iterations="${SANITIZERS_STRESS_ITERATIONS:-10000}"
sanitizer_color="${SANITIZERS_COLOR:-always}"

case "${sanitizer_color}" in
auto | always | never) ;;
*)
    echo "SANITIZERS_COLOR must be auto, always, or never" >&2
    exit 2
    ;;
esac

tmp_dir="$(mktemp -d)"
report_dir="${tmp_dir}/reports"

print_sanitizer_reports() {
    local found=0
    local report

    for report in "${report_dir}"/*; do
        if [[ ! -f "${report}" || ! -s "${report}" ]]; then
            continue
        fi
        found=1
        printf '\n[sanitizers] report: %s\n' "$(basename "${report}")"
        cat "${report}"
        printf '[sanitizers] end report\n'
    done

    if [[ ${found} -eq 0 ]]; then
        echo "[sanitizers] no ASan, LSan, or UBSan findings"
        return 0
    fi
    return 1
}

restore_normal_build() {
    local status=$?
    local restore_status=0

    trap - EXIT
    if ! print_sanitizer_reports && [[ ${status} -eq 0 ]]; then
        status=1
    fi
    rm -rf "${tmp_dir}"
    if [[ "${restore_build}" == "1" ]]; then
        echo "[sanitizers] restoring normal NIF build"
        set +e
        make -C "${c_src_dir}" clean CC=clang CXX=clang++
        make -C "${c_src_dir}" CC=clang CXX=clang++
        restore_status=$?
        set -e
        if [[ ${status} -eq 0 && ${restore_status} -ne 0 ]]; then
            status=${restore_status}
        fi
    fi
    exit "${status}"
}
trap restore_normal_build EXIT

mkdir -p "${report_dir}"

if [[ -n "${SANITIZERS_LSAN_SUPPRESSIONS:-}" ]]; then
    if [[ ! -f "${SANITIZERS_LSAN_SUPPRESSIONS}" ]]; then
        echo "missing LSan suppression file: ${SANITIZERS_LSAN_SUPPRESSIONS}" >&2
        exit 1
    fi
    cat "${SANITIZERS_LSAN_SUPPRESSIONS}" > "${tmp_dir}/lsan.supp"
fi
printf 'leak:inet_gethost\n' >> "${tmp_dir}/lsan.supp"

emu_type="$({ ASAN_OPTIONS=detect_leaks=0 erl -emu_type asan -noshell \
    -eval 'io:put_chars(erlang:atom_to_list(erlang:system_info(emu_type))), halt().' ; } 2>/dev/null || true)"
if [[ "${emu_type}" != "asan" ]]; then
    echo "an Erlang/OTP ASan emulator is required; use 'just sanitizers-docker'" >&2
    exit 1
fi
echo "[sanitizers] using Erlang/OTP ASan emulator"

echo "[sanitizers] building NIF with ASan, LSan, and UBSan (excluding packed-vterm alignment checks)"
arch="$(uname -m)"
ubsan_lib="$(clang -print-file-name="libclang_rt.ubsan_standalone-${arch}.so")"
ubsan_dir="$(dirname "${ubsan_lib}")"
if [[ ! -f "${ubsan_lib}" ]]; then
    echo "missing Clang sanitizer runtime for ${arch}" >&2
    exit 1
fi

# The vterm arena deliberately packs variable-sized objects. Keep the useful
# undefined-behavior checks without reporting that existing layout design.
sanitize_flags='-fsanitize=address,undefined -fno-sanitize=alignment -fno-common -fno-omit-frame-pointer -fno-sanitize-recover=undefined'
sanitize_ldflags="-fsanitize=address ${ubsan_lib} -Wl,-rpath,${ubsan_dir}"
make -C "${c_src_dir}" clean CC=clang CXX=clang++
make -C "${c_src_dir}" \
    SANITIZE=1 \
    CC=clang \
    CXX=clang++ \
    CFLAGS_SANITIZE="${sanitize_flags}" \
    CXXFLAGS_SANITIZE="${sanitize_flags}" \
    LDFLAGS="${sanitize_ldflags}"

sanitizer_env=(
    env
    "ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1:color=${sanitizer_color}:log_path=${report_dir}/asan-erldist_filter"
    "LSAN_OPTIONS=suppressions=${tmp_dir}/lsan.supp:print_suppressions=0:exitcode=23:color=${sanitizer_color}"
    "UBSAN_OPTIONS=halt_on_error=1:print_stacktrace=1:color=${sanitizer_color}:log_path=${report_dir}/ubsan-erldist_filter"
)
if symbolizer="$(command -v llvm-symbolizer || command -v llvm-symbolizer-19 || true)" && [[ -n "${symbolizer}" ]]; then
    sanitizer_env+=("ASAN_SYMBOLIZER_PATH=${symbolizer}")
fi

erl_aflags="${ERL_AFLAGS:-}"
if [[ -n "${erl_aflags}" ]]; then
    erl_aflags+=" "
fi
erl_aflags+="-emu_type asan"

echo "[sanitizers] running Common Test with the ASan emulator"
(
    cd "${project_root}"
    "${sanitizer_env[@]}" \
        "ERL_AFLAGS=${erl_aflags}" \
        ERLDIST_FILTER_REQUIRE_ASAN=1 \
        SANITIZE=1 \
        CC=clang \
        CXX=clang++ \
        "CFLAGS_SANITIZE=${sanitize_flags}" \
        "CXXFLAGS_SANITIZE=${sanitize_flags}" \
        "LDFLAGS=${sanitize_ldflags}" \
        rebar3 ct
)

code_paths=()
for path in "${project_root}"/_build/test/lib/*/ebin "${project_root}/_build/test/lib/erldist_filter_test/test"; do
    if [[ -d "${path}" ]]; then
        code_paths+=(-pa "${path}")
    fi
done

stress_code="
Atom = list_to_atom(\"b@b\"),
io:format(\"[sanitizers] cleanup stress (~B iterations)~n\", [${stress_iterations}]),
lists:foreach(
    fun(_) ->
        {a, b, c} = erldist_filter_nif:dist_ext_to_term({a, b, c}, <<131, 104, 3, 82, 0, 82, 1, 82, 2>>),
        Pid = erldist_filter_nif:dist_ext_to_term({Atom}, <<131, 88, 82, 0, 0:32, 0:32, 0:32>>),
        true = is_pid(Pid),
        _ = (catch erldist_filter_nif:dist_ext_to_term({a}, <<131, 104, 2, 82, 0>>))
    end,
    lists:seq(1, ${stress_iterations})
),
erlang:garbage_collect(),
halt().
"

echo "[sanitizers] running cleanup stress"
(
    cd "${project_root}"
    "${sanitizer_env[@]}" erl -emu_type asan -noshell "${code_paths[@]}" -eval "${stress_code}"
)
