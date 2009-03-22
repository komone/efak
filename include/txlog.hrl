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

-record(txlog, {
			tx_key,		%% unique key for the transaction
			ts,			%% now() tuple representing last log operation
			elapsed,	%% integer(), elapsed milleseconds between request and response
			interface,	%% atom(), name of the interface that logged the transaction
			bday,		%% date()
			type,		%% atom(), values are data | timeout | line_down
			tx 			%% isomsg(), containing response and request fields with
						%% precedence on response ones
			}).


