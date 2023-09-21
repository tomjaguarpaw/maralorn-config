-- You can define your global state here
local cols = 3
local maxsubcols = 2
local maxrows = 2

-- The most important function - the actual layout generator
--
-- The argument is a table with:
--  * Focused tags (`args.tags`)
--  * Window count (`args.count`)
--  * Output width (`args.width`)
--  * Output height (`args.height`)
--  * Output name (`args.output`)
--
-- The return value must be a table with exactly `count` entries. Each entry is a table with four
-- numbers:
--  * X coordinate
--  * Y coordinate
--  * Window width
--  * Window height
--
-- This example is a simplified version of `rivertile`

function div(x,y)
  return math.floor(x / y)
end

function col_count(count, col)
		local num = div(count-1,cols)

		if count <= cols and col - 1< count then
			num = num + 1
		end
		if count > cols and cols - ((count - 1) % cols + 1) <= col - 1 then
			num = num + 1
		end

		return num
end

function handle_layout(args)
	local colorder = {}
	local colwidth = div(args.width,cols)
	for i = 1, cols do
		if i % 2 == 0 then
			table.insert(colorder,div(cols - 1,2) + (div(i, 2) * (1 - 2 * (cols % 2))))
		else
			table.insert(colorder,div(cols - 1,2) - (div(i, 2) * (1 - 2 * (cols % 2))))
		end
	end

	local retval = {}
	for i = 1, cols do
		local count = col_count(args.count, i)
	  if count <= maxsubcols then
		  local subcolwidth = div(colwidth,count)
		  for col = 0, count - 1 do
				table.insert(retval, {
					colorder[i] * colwidth + subcolwidth * col,
					0,
					subcolwidth,
					args.height
				})
			end
	  else
		  local subcolwidth = div(colwidth,maxsubcols)
		  for col = 0, maxsubcols - 1 do
			  local rows = math.min(div(count,maxsubcols),maxrows)
			  if rows < maxrows and count % maxsubcols > col then
			    rows = rows + 1
			  end
			  local cellheight = div(args.height,rows)
			  for row = 0, rows - 1 do
					table.insert(retval, {
						colorder[i] * colwidth + subcolwidth * col,
						cellheight * row,
						subcolwidth,
						cellheight
					})
				end
			end
		end
	end
	for i = 1, args.count - cols * maxsubcols * maxrows do
	  local subcolwidth = div(colwidth,maxsubcols)
    local cellheight = div(args.height,maxrows)
		table.insert(retval, {
			(cols - 1) * colwidth + (maxsubcols - 1) * subcolwidth,
			cellheight * (maxrows - 1),
			subcolwidth,
			cellheight
		})
	end
	return retval
end

--local foo = handle_layout({width = 1000, height = 1000, count = 14})

--print(#foo)

-- This optional function returns the metadata for the current layout.
-- Currently only `name` is supported, the name of the layout. It get's passed
-- the same `args` as handle_layout()
function handle_metadata(args)
	return { name = "rivertile-simple" }
end

-- IMPORTANT: User commands send via `riverctl send-layout-cmd` are treated as lua code.
-- Active tags are stored in `CMD_TAGS` global variable.
-- Output name is stored in `CMD_OUTPUT` global variable.

-- Here is an example of a function that can be mapped to some key
-- Run with `riverctl send-layout-cmd luatile "toggle_gaps()"`
-- local gaps_alt = 0
-- function toggle_gaps()
-- 	local tmp = gaps
-- 	gaps = gaps_alt
-- 	gaps_alt = tmp
-- end
