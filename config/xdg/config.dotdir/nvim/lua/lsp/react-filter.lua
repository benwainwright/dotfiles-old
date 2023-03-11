-- Borrowed from https://github.com/typescript-language-server/typescript-language-server/issues/216#issuecomment-1001585748
local react_filter = function(_, result, params)
    if result == nil or vim.tbl_isempty(result) then
        local _ = vim.lsp.log and vim.lsp.log.info and
                      vim.lsp.log.info(params.method, 'No location found')
        return nil
    end

    if vim.tbl_islist(result) then
        vim.lsp.util.jump_to_location(result[1], 'utf-8')
        if #result > 1 then
            local isReactDTs = false
            for _, value in pairs(result) do
                print(vim.inspect(value))
                if string.match(value.targetUri, "react/index.d.ts") then
                    isReactDTs = true
                    break
                end
            end
            if not isReactDTs then
                vim.fn.setqflist(vim.lsp.util.locations_to_items(result))
                vim.api.nvim_command("copen")
            end
        end
    else
        vim.lsp.util.jump_to_location(result, 'utf-8')
    end
end

return react_filter
