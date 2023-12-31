%% NOTE: This file is imported from https://raw.githubusercontent.com/erlang/otp/OTP-25.2.3/lib/kernel/include/net_address.hrl
%% @oncall whatsapp_clr

%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
%% Copyright (c) Meta Platforms, Inc. and affiliates.
%% Copyright (c) WhatsApp LLC
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

%% Generic address format

-record(net_address,
	{
	 address,  %% opaque address
	 host,     %% host name
	 protocol, %% protocol
	 family    %% address family
	}).
