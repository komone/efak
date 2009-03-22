{application, efak,
	[{description, "Erlang Financial Application Kit."},
	{vsn, "0.1"},
	{modules, [config_server, efak_app, efak_ctl, efak_sup, erl_interface,
				erl_interface_sup, gen_server_cluster, ilb,	interfaces_sup,
				interface_svc_sup, iso, iso_interface, iso_interface_sup,
				iso_log, iso_saf, iso_test, iso_transaction, iso_util,
				key_timers, line_proxy, lines_sup, line_sup, simple_worker_sup,
				tcp_acceptor, tcp_ch_connection, tcp_ch_connector, 
				tcp_connection, tcp_connector, transaction_controller, 
				transaction_mgr]},
	{registered, [config_server, efak_sup, erl_interface, erl_interface_sup,
				ilb, interfaces_sup, interface_svc_sup, iso_interface_sup,
				iso_saf, key_timers, line_proxy, lines_sup, line_sup,
				simple_worker_sup, transaction_controller, transaction_mgr]},
	{applications, [kernel, stdlib, sasl]},
	{mod, {efak_app, []}},
	{env, [
	 	{config_server, {file, "./efakconf.xml"}}
	]}
]}.

