{application, bg_example,
	[{description, "efak bg_example"},
	{vsn, "0.1"},
	{modules, [bg_auth_tx, bg_example, bg_example_app, bg_example_sup,
				bg_network_tx, bg_reversal_tx, bg_security]},
	{registered, [bg_example_sup]},
	{applications, [kernel, stdlib, sasl]},
	{mod, {bg_example_app, []}},
	{env, []}
]}.

