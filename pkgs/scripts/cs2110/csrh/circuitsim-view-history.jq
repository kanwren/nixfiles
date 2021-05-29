# Split array into groups of size n
def groups(n): . as $arr | n as $n | length as $len
    | [range(0; $len; $n) | [range(.; . + $n)]]
    | map(map($arr[.]))
;

# Format a Unix timestamp in milliseconds
def millis_to_date: tonumber | . / 1000 | localtime | strftime("%a %h %d %Y %H:%M:%S %Z");

# Split a base64-encoded revision signature into its components
def decode_signature: @base64d / "\t";

# Parse a block out of the components of a signature/copy block
def extract_block: . as $b | {
    previous_block_hash: .[0],
    current_block_hash: .[1],
    timestamp: .[2] | millis_to_date,
    file_data_hash: .[3],
} | if $verbose then (.timestamp_raw = $b[2]) else . end;

# Decode and parse a block signature
def decode_extract_block: . as $s
    | decode_signature
    | . as $i
    | extract_block
    | if $verbose then ((.signature = $s) | (.signature_parts = $i)) else . end
;

# Parse an array of copy block signatures and split into start/middle/end components
def copied_blocks: groups(3) | map(
    map(decode_extract_block)
    | {
        start_signature: .[0],
        middle_signature: .[1],
        end_signature: .[2],
    }
);

# Decode and parse a revision signature
def decode_extract_signature: . as $s
    | decode_signature
    | . as $i
    | extract_block
    | if $verbose then ((.signature = $s) | (.signature_parts = $i)) else . end
    | (.copied_signatures = ($i[4:] | copied_blocks))
;

(.revisionSignatures // []) | map(decode_extract_signature)
