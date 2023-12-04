-- AOC 2023 day 4 in Lua

local score = 0
local multipliers = {}
local totalCards = 0

for line in io.lines("input") do
	local _, _, cardNr, a, b = string.find(line, "^Card +(%d+): ([^|]*)|(.*)$")
	cardNr = tonumber(cardNr)
	local cardCount = multipliers[cardNr] or 1
	totalCards = totalCards + cardCount

	local winningNrs = {}
	for s in string.gmatch(a, "%d+") do winningNrs[tonumber(s)] = true end

	local matchCount = 0
	for s in string.gmatch(b, "%d+") do
		local nr = tonumber(s)
		if winningNrs[nr] then matchCount = matchCount + 1 end
	end
	for i = cardNr + 1, cardNr + matchCount do
		multipliers[i] = (multipliers[i] or 1) + cardCount
	end
	if matchCount > 0 then score = score + bit.lshift(1, matchCount - 1) end
end

print("Score:", score, "Card count:", totalCards)
