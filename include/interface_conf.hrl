%% efak - erlang financial application kit
%% Copyright (C) 2009  Paolo Montrasi
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Affero General Public License as
%% published by the Free Software Foundation, either version 3 of the
%% License, or (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU Affero General Public License for more details.
%%
%% You should have received a copy of the GNU Affero General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>.

-record(interface_conf, {
			%% configuration parameters
			name,				%% atom()
			iso_version,		%% $0 | $1 | $2
			validator,			%% [{MTI, Mandatory, Optional}]
			packer,				%% {Packer, Unpacker}
			pool_size,			%% int()
			callback,			%% {M,F,A} to deliver messages as apply(M,F,A ++ Msg)
			security_module,	%% module that implements calc_mac(Msg)

			%% interface parameters
			status,				%% up | down
			sign_on,			%% false | true
			local_inst_id,		%% string() | any
			remote_inst_id,		%% string() | any
			cut_off,			%% time()
			currency,			%% int()
			timeout, 			%% int() milliseconds to wait for a response

			%% saf parameters
			retry,				%% int()
			repeat_timeout,		%% int()
			
			%% internal state
			current_date		%% date()
			}).

