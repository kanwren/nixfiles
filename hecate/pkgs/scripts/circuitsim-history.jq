# Split array into groups of size n
def groups(n): . as $arr | n as $n | length as $len
    | [range(0; $len; $n) | [range(.; . + $n)]]
    | map(map($arr[.]))
;

# Format a Unix timestamp in milliseconds
def millisToDate: tonumber | . / 1000 | localtime | strftime("%a %h %d %Y %H:%M:%S %Z");

# Split a base64-encoded revision signature into its components
def decodeSignature: @base64d / "\t";

# Parse a block out of the components of a signature/copy block
def extractBlock: {
    previous_block_hash: .[0],
    current_block_hash: .[1],
    time_stamp: .[2] | millisToDate,
    file_data_hash: .[3],
};

# Parse an array of copy block signatures and split into start/middle/end components
def copiedBlocks: groups(3) | map(
    map(decodeSignature | extractBlock)
    | {
        start_signature: .[0],
        middle_signature: .[1],
        end_signature: .[2],
    }
);

# Parse a revision signature
def extractSignature: . as $i
    | extractBlock
    | (.copied_signatures = ($i[4:] | copiedBlocks))
;

(.revisionSignatures // []) | map(decodeSignature | extractSignature)
