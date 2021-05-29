($hashes | split(":")) as $split_hashes |

# Add an index to every element in the array
def with_indices: . as $arr | length as $len | [range(0; $len) | { index: ., value: $arr[.] }];

# Take an array and turn it into an array of all adjacent (overlapping) pairs,
# each represented as an object { _1: <first>, _2: <second> }
def pairs: [.[:-1], .[1:]] | transpose | map({ _1: .[0], _2: .[1] });

def check_pair: ._1.index as $ix1 | ._2.index as $ix2
    | ._1.value.current_block_hash as $current | ._2.value.previous_block_hash as $previous
    | "Hash mismatch between revisions \($ix1) and \($ix2): previous block hash of revision \($ix2) is \($previous), but block hash of revision \($ix1) is \($current)\n" as $msg
    | if $current == $previous then { success: true } else { success: false, msg: $msg } end
;

def combine_results:
    if all(.success) then
        { success: true }
    else
        { sucess: false, msg: map(.msg) | add }
    end
;

def check_pairs: pairs | map(check_pair) | combine_results;

def check_first_block: .[0].index as $ix | .[0].value.previous_block_hash as $hash
    | if $hash == "" then
        { success: true }
    else
        { success: false, msg: "Hash mismatch: expected an empty previous block hash at index \($ix), but got hash \($hash)\n" }
    end
;

def check_block_hash: .index as $ix | .value.current_block_hash as $hash | $split_hashes[$ix] as $expected
    | if $hash == $expected then
        { success: true }
    else
        { success: false, msg: "Hash mismatch: expected block hash \($expected) at index \($ix), but got hash \($hash)\n" }
    end
;

def check_block_hashes: map(check_block_hash) | combine_results;

def all_checks: [check_first_block, check_pairs, check_block_hashes] | combine_results;

def report_errors: if .success then empty else .msg | halt_error(1) end;

if length == 0 then
    "Empty revision history\n" | halt_error(1)
else
    with_indices | all_checks | report_errors
end

