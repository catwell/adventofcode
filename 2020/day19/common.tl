local util = require "util"

local enum RuleKind
    "letter"
    "seq"
    "alternative"
    "custom"
end

local type Seq = {number}

local record Rule
    kind: RuleKind
    letter: string
    seq: Seq
    seq2: Seq
end

local type Rules = {number:Rule}

local type MsgSet = {string: boolean}

local type MatchCache = {string: MsgSet}

local M = {}

local function parse_rules(ss: {string}): Rules
    local r : Rules = {}
    for _, s in ipairs(ss) do
        if s:find('"') then
            local id, letter = s:match("^(%d+): \"([ab])\"$")
            r[tonumber(id)] = { kind = "letter", letter = letter }
        elseif s:find("|") then
            local id, nums, nums2 = s:match("^(%d+): ([%d%s]+) | ([%d%s]+)$")
            r[tonumber(id)] = {
                kind = "alternative",
                seq = util.parse_integers(nums),
                seq2 = util.parse_integers(nums2),
            }
        else
            local id, nums = s:match("^(%d+): ([%d%s]+)$")
            r[tonumber(id)] = {
                kind = "seq",
                seq = util.parse_integers(nums),
            }
        end
    end
    return r
end

local rule_match: function(rules: Rules, id: number, msgs: MsgSet, matched: MatchCache): MsgSet

local function rule_match_seq(rules: Rules, seq: Seq, msg: string, matched: MatchCache): MsgSet
    local r = {[msg] = true}
    for _, id in ipairs(seq) do
        r = rule_match(rules, id, r, matched)
    end
    return r
end

rule_match = function(rules: Rules, id: number, msgs: MsgSet, matched: MatchCache): MsgSet
    local r = {}
    for msg in pairs(msgs) do
        local rule = rules[id]
        local ref = msg .. tostring(id)
        if not matched[ref] then
            local r2 = {}
            if rule.kind == "letter" then
                if msg:sub(1, 1) == rule.letter then
                    r2[msg:sub(2)] = true
                end
            elseif rule.kind == "seq" then
                util.set_add(r2, rule_match_seq(rules, rule.seq, msg, matched))
            else
                util.set_add(r2, rule_match_seq(rules, rule.seq, msg, matched))
                util.set_add(r2, rule_match_seq(rules, rule.seq2, msg, matched))
            end
            matched[ref] = r2
        end
        util.set_add(r, matched[ref])
    end
    return r
end

function M.parse_input() : Rules, MsgSet
    local input = util.split(util.read_file("input.txt"), "\n\n")
    local rules = parse_rules(util.split(input[1], "\n"))
    local msgs = util.to_set(util.split(input[2], "\n"))
    return rules, msgs
end

function M.count_matching(rules: Rules, msgs: MsgSet): number
    local n, matched = 0, {}
    for msg in pairs(msgs) do
        local r = rule_match(rules, 0, {[msg] = true}, matched)
        for v in pairs(r) do
            if #v == 0 then
                n = n + 1
                break
            end
        end
    end
    return n
end

function M.fix_rules(rules: Rules)
    rules[8] = {
        kind = "alternative",
        seq = {42},
        seq2 = {42, 8},
    }
    rules[11] = {
        kind = "alternative",
        seq = {42, 31},
        seq2 = {42, 11, 31},
    }
end

return M
