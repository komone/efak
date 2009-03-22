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

-record(imsg, {
			cmd = data,	%% data | timeout | line_down | interface | ...
			src,		%% source of the message  
						%% { interface_name , pid }
						%% { module_name , pid }
						%% pid is the pid of the line process or undefined
			dst,		%% destinatio of the message  
						%% { interface_name , pid }
						%% { module_name , pid }
						%% pid is the pid of the line process or undefined
			msg = [],	%% list of {field_id, value} tuples
			header = []	%% optional message header
			}).
