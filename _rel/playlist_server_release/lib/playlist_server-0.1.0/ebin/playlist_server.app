{application, 'playlist_server', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['hello_handler','playlist_handler','playlist_server_app','playlist_server_sup']},
	{registered, [playlist_server_sup]},
	{applications, [kernel,stdlib,cowboy]},
	{mod, {playlist_server_app, []}},
	{env, []}
]}.