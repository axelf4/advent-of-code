# AOC 2023 day 10 in Julia

input = mapreduce(x -> permutedims(collect(x)), vcat, readlines("input"))
start = findfirst(x -> x == 'S', input)

struct PipeIterator
    start::CartesianIndex
    first::CartesianIndex
end

function Base.iterate(I::PipeIterator, (prevx, x)=(I.start, I.first))
    if !checkbounds(Bool, input, x) return nothing end
    ch = input[x]
    i, j = Tuple(x)
    next = if ch == '|'
        (CartesianIndex(i - 1, j), CartesianIndex(i + 1, j))
    elseif ch == '-'
        (CartesianIndex(i, j - 1), CartesianIndex(i, j + 1))
    elseif ch == 'L'
        (CartesianIndex(i - 1, j), CartesianIndex(i, j + 1))
    elseif ch == 'J'
        (CartesianIndex(i, j - 1), CartesianIndex(i - 1, j))
    elseif ch == '7'
        (CartesianIndex(i, j - 1), CartesianIndex(i + 1, j))
    elseif ch == 'F'
        (CartesianIndex(i, j + 1), CartesianIndex(i + 1, j))
    elseif ch == '.' return nothing
    elseif ch == 'S' return if prevx == x nothing else (x, (x, x)) end
    else error("Unreachable")
    end
    y = if next[1] == prevx next[2]
    elseif next[2] == prevx next[1]
    else return nothing
    end
    (x, (x, y))
end

Base.IteratorSize(::Type{PipeIterator}) = Base.SizeUnknown()

function findloop(start)
    for (dx, dy) in [(-1, 0), (1, 0), (0, -1), (0, 1)]
        xs = collect(PipeIterator(start, start + CartesianIndex(dy, dx)))
        if !isempty(xs) && xs[end] === start return xs end
    end
end

xs = findloop(start)
println("Solution 1: ", length(xs) ÷ 2)

function floodfill(start, border)
    set = Set([start])
    queue = [start]
    while !isempty(queue)
        x = pop!(queue)
        for (dx, dy) in [(-1, 0), (1, 0), (0, -1), (0, 1)]
            y = x + CartesianIndex(dy, dx)
            if checkbounds(Bool, input, y) && y ∉ set && y ∉ border
                push!(set, y)
                push!(queue, y)
            end
        end
    end
    set
end

leftset = Set()
rightset = Set()
loopset = Set(xs)
prev = start
for x in xs
    diff = x - prev
    dx = CartesianIndex(diff[2], -diff[1]) # Rotate 90 degrees clockwise

    for left in (x - dx, prev - dx)
        if checkbounds(Bool, input, left) && left ∉ leftset && left ∉ loopset
            union!(leftset, floodfill(left, loopset))
        end
    end
    for right in (x + dx, prev + dx)
        if checkbounds(Bool, input, right) && right ∉ rightset && right ∉ loopset
            union!(rightset, floodfill(right, loopset))
        end
    end

    global prev = x
end

innerset = if all(x -> checkbounds(Bool, input[2:end - 1, 2:end - 1], x), leftset) && !isempty(leftset)
    leftset
else
    rightset
end
println("Solution 2: ", length(innerset))
