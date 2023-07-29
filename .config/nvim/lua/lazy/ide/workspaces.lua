return {
    {
        "natecraddock/workspaces.nvim",
        cmd = {
            "WorkspacesAdd", "WorkspacesList", "WorkspacesOpen",
            "WorkspacesAddDir", "WorkspacesRemove", "WorkspacesRename",
            "WorkspacesListDirs", "WorkspacesSynsDirs", "WorkspacesRemoveDir"
        },
        keys = { "<Leader>sp", "[S]earch [P]rojects"},
        dependencies = { "nvim-telescope/telescope.nvim" },
        config = function()
            require('plugins.telescope').workspaces()
            require('plugins.telescope').workspaces_keys()
        end
    },
}
