-- You can define your global state here

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


function handle_layout(args)
  local cols = 3
  local maxrows = 2
  local maxsubcols = 2
	if args.width < 4500 then
	  cols = 1
  end
	if 3500 < args.width and args.width < 4500 then
	  cols = 2
  end
	if args.height < 1200 then
	  maxrows = 1
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
	local totalcount = args.count
	if cols > 1 then
		totalcount = totalcount - 1
	end
	for i = 1, cols do
		local height = args.height
		local voffset = 0
		if i == 1 and cols > 1 then
			voffset = height / 2
			height = height / 2
		end
		local count = col_count(totalcount, i)
	  if count <= maxsubcols then
		  local subcolwidth = div(colwidth,count)
		  for col = 0, count - 1 do
				table.insert(retval, {
					colorder[i] * colwidth + subcolwidth * col,
					voffset,
					subcolwidth,
					height
				})
			end
	  else
		  local subcolwidth = div(colwidth,maxsubcols)
		  for col = 0, maxsubcols - 1 do
			  local rows = math.min(div(count,maxsubcols),maxrows)
			  if rows < maxrows and count % maxsubcols > col then
			    rows = rows + 1
			  end
			  local cellheight = div(height,rows)
			  for row = 0, rows - 1 do
					table.insert(retval, {
						colorder[i] * colwidth + subcolwidth * col,
						voffset + cellheight * row,
						subcolwidth,
						cellheight
					})
				end
			end
		end
	end
	local subcolwidth = div(colwidth,maxsubcols)
  local cellheight = div(args.height,maxrows)
	for i = 1, totalcount - cols * maxsubcols * maxrows do
		table.insert(retval, {
			(cols - 1) * colwidth + (maxsubcols - 1) * subcolwidth,
			cellheight * (maxrows - 1),
			subcolwidth,
			cellheight
		})
	end
	if cols > 1 and totalcount >= 0 then
		table.insert(retval, {
			0,
			0,
			colwidth,
			cellheight
		})
	end
	return retval
end

-- local foo = handle_layout({width = 3840, height = 2160, count = 1})

-- print(#foo)
-- print(foo[1][4])

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
