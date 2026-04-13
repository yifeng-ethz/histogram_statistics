package require Tcl 8.5

set script_dir [file dirname [info script]]
set helper_file [file normalize [file join $script_dir .. dashboard_infra cmsis_svd lib mu3e_cmsis_svd.tcl]]
source $helper_file

namespace eval ::mu3e::cmsis::spec {}

proc ::mu3e::cmsis::spec::build_device {} {
    set registers [list \
        [::mu3e::cmsis::svd::register UID 0x00 \
            -description {Software-visible IP identifier. Default ASCII "HIST" (0x48495354).} \
            -access read-only \
            -resetValue 0x48495354 \
            -fields [list \
                [::mu3e::cmsis::svd::field value 0 32 \
                    -description {Compile-time or integration-time UID word. Default ASCII "HIST".} \
                    -access read-only]]] \
        [::mu3e::cmsis::svd::register META 0x04 \
            -description {Read-multiplexed metadata word. Write meta_sel[1:0] before reading back: 0=VERSION, 1=DATE, 2=GIT, 3=INSTANCE_ID.} \
            -access read-write \
            -fields [list \
                [::mu3e::cmsis::svd::field meta_sel 0 2 -description "Selects the META read page." -access read-write] \
                [::mu3e::cmsis::svd::field reserved 2 30 -description "Reserved, read as zero on selector writeback." -access read-only]]] \
        [::mu3e::cmsis::svd::register CONTROL 0x08 \
            -description "Configuration apply, mode, key interpretation, filter control, and validation status." \
            -access read-write \
            -resetValue 0x00000100 \
            -fields [list \
                [::mu3e::cmsis::svd::field apply 0 1 -description "Write 1 to request that the staged configuration becomes active after the ingress path drains." -access read-write] \
                [::mu3e::cmsis::svd::field apply_pending 1 1 -description "1 while a committed configuration is waiting to settle into the live datapath." -access read-only] \
                [::mu3e::cmsis::svd::field reserved0 2 2 -description "Reserved, read as zero." -access read-only] \
                [::mu3e::cmsis::svd::field mode 4 4 -description "Mode selector. Negative 4-bit signed values route one of the debug inputs into the histogram path." -access read-write] \
                [::mu3e::cmsis::svd::field key_unsigned 8 1 -description "1 selects unsigned update-key interpretation, 0 selects signed extraction." -access read-write] \
                [::mu3e::cmsis::svd::field reserved1 9 3 -description "Reserved, read as zero." -access read-only] \
                [::mu3e::cmsis::svd::field filter_enable 12 1 -description "Enables the runtime filter-key comparison." -access read-write] \
                [::mu3e::cmsis::svd::field filter_reject 13 1 -description "0 accepts matching events, 1 rejects matching events." -access read-write] \
                [::mu3e::cmsis::svd::field reserved2 14 10 -description "Reserved, read as zero." -access read-only] \
                [::mu3e::cmsis::svd::field error 24 1 -description "Set when the last apply request failed CSR validation." -access read-only] \
                [::mu3e::cmsis::svd::field reserved3 25 3 -description "Reserved, read as zero." -access read-only] \
                [::mu3e::cmsis::svd::field error_info 28 4 -description "Validation error code. 0x1 indicates invalid bounds in auto-right-bound mode." -access read-only]]] \
        [::mu3e::cmsis::svd::register LEFT_BOUND 0x0C \
            -description "Signed left boundary of the histogram range." \
            -access read-write \
            -fields [list [::mu3e::cmsis::svd::field value 0 32 -description "Signed left boundary." -access read-write]]] \
        [::mu3e::cmsis::svd::register RIGHT_BOUND 0x10 \
            -description "Signed right boundary of the histogram range. Recomputed at apply time when BIN_WIDTH != 0." \
            -access read-write \
            -fields [list [::mu3e::cmsis::svd::field value 0 32 -description "Signed right boundary." -access read-write]]] \
        [::mu3e::cmsis::svd::register BIN_WIDTH 0x14 \
            -description "Bin width in key-space units. Set to 0 to keep explicit left and right bounds." \
            -access read-write \
            -fields [list \
                [::mu3e::cmsis::svd::field value 0 16 -description "Bin width in key-space units." -access read-write] \
                [::mu3e::cmsis::svd::field reserved 16 16 -description "Reserved, read as zero." -access read-only]]] \
        [::mu3e::cmsis::svd::register KEY_LOC 0x18 \
            -description "Packed bit-slice locations for update-key and filter-key extraction." \
            -access read-write \
            -resetValue 0x26231D11 \
            -fields [list \
                [::mu3e::cmsis::svd::field update_key_low 0 8 -description "LSB of the update-key slice inside the snooped data word." -access read-write] \
                [::mu3e::cmsis::svd::field update_key_high 8 8 -description "MSB of the update-key slice inside the snooped data word." -access read-write] \
                [::mu3e::cmsis::svd::field filter_key_low 16 8 -description "LSB of the filter-key slice inside the snooped data word." -access read-write] \
                [::mu3e::cmsis::svd::field filter_key_high 24 8 -description "MSB of the filter-key slice inside the snooped data word." -access read-write]]] \
        [::mu3e::cmsis::svd::register KEY_VALUE 0x1C \
            -description "Packed runtime key overrides used by mode-dependent histogram logic." \
            -access read-write \
            -fields [list \
                [::mu3e::cmsis::svd::field update_key 0 16 -description "Literal update-key override." -access read-write] \
                [::mu3e::cmsis::svd::field filter_key 16 16 -description "Literal filter-key comparison value." -access read-write]]] \
        [::mu3e::cmsis::svd::register UNDERFLOW_COUNT 0x20 \
            -description "Count of keys mapped below the configured range." \
            -access read-only \
            -fields [list [::mu3e::cmsis::svd::field value 0 32 -description "Underflow event count." -access read-only]]] \
        [::mu3e::cmsis::svd::register OVERFLOW_COUNT 0x24 \
            -description "Count of keys mapped above the configured range." \
            -access read-only \
            -fields [list [::mu3e::cmsis::svd::field value 0 32 -description "Overflow event count." -access read-only]]] \
        [::mu3e::cmsis::svd::register INTERVAL_CFG 0x28 \
            -description "Ping-pong interval timer configuration in clock cycles." \
            -access read-write \
            -fields [list [::mu3e::cmsis::svd::field value 0 32 -description "Interval length in clock cycles." -access read-write]]] \
        [::mu3e::cmsis::svd::register BANK_STATUS 0x2C \
            -description "Ping-pong bank-selection and flush-progress status." \
            -access read-only \
            -fields [list \
                [::mu3e::cmsis::svd::field active_bank 0 1 -description "Currently active write bank." -access read-only] \
                [::mu3e::cmsis::svd::field flushing 1 1 -description "1 while the next active bank is being cleared." -access read-only] \
                [::mu3e::cmsis::svd::field reserved0 2 6 -description "Reserved, read as zero." -access read-only] \
                [::mu3e::cmsis::svd::field flush_addr 8 8 -description "Bin index currently being cleared." -access read-only] \
                [::mu3e::cmsis::svd::field reserved1 16 16 -description "Reserved, read as zero." -access read-only]]] \
        [::mu3e::cmsis::svd::register PORT_STATUS 0x30 \
            -description "Ingress FIFO empty-mask and maximum observed fill level." \
            -access read-only \
            -fields [list \
                [::mu3e::cmsis::svd::field fifo_empty_mask 0 8 -description "One bit per ingress FIFO. 1 indicates the corresponding FIFO is empty." -access read-only] \
                [::mu3e::cmsis::svd::field reserved0 8 8 -description "Reserved, read as zero." -access read-only] \
                [::mu3e::cmsis::svd::field fifo_level_max 16 8 -description "Maximum observed FIFO fill level across the ingress-port pairs." -access read-only] \
                [::mu3e::cmsis::svd::field reserved1 24 8 -description "Reserved, read as zero." -access read-only]]] \
        [::mu3e::cmsis::svd::register TOTAL_HITS 0x34 \
            -description "Total accepted hit count across all active sources." \
            -access read-only \
            -fields [list [::mu3e::cmsis::svd::field value 0 32 -description "Accepted-hit count." -access read-only]]] \
        [::mu3e::cmsis::svd::register DROPPED_HITS 0x38 \
            -description "Total dropped hits caused by FIFO or queue overflow." \
            -access read-only \
            -fields [list [::mu3e::cmsis::svd::field value 0 32 -description "Dropped-hit count." -access read-only]]] \
        [::mu3e::cmsis::svd::register COAL_STATUS 0x3C \
            -description "Coalescing-queue occupancy, occupancy maximum, and overflow count." \
            -access read-only \
            -fields [list \
                [::mu3e::cmsis::svd::field queue_occupancy 0 8 -description "Current coalescing-queue occupancy." -access read-only] \
                [::mu3e::cmsis::svd::field queue_occupancy_max 8 8 -description "Maximum queue occupancy observed in the current interval." -access read-only] \
                [::mu3e::cmsis::svd::field queue_overflow_count 16 16 -description "Total number of queue-overflow events." -access read-only]]] \
        [::mu3e::cmsis::svd::register SCRATCH 0x40 \
            -description "General-purpose scratch register for integration testing." \
            -access read-write \
            -fields [list [::mu3e::cmsis::svd::field value 0 32 -description "Scratch value." -access read-write]]]]

    return [::mu3e::cmsis::svd::device MU3E_HISTOGRAM_STATISTICS \
        -version 26.1.0.0411 \
        -description "CMSIS-SVD description of the histogram statistics CSR window. BaseAddress is 0 because this file describes the relative CSR aperture of the IP; system integration supplies the live slave base address." \
        -peripherals [list \
            [::mu3e::cmsis::svd::peripheral HISTOGRAM_STATISTICS_CSR 0x0 \
                -description "Relative 17-word CSR aperture for histogram statistics. The separate hist_bin data window is intentionally excluded because it is histogram memory, not control/status registers." \
                -groupName MU3E_HISTOGRAM_STATISTICS \
                -addressBlockSize 0x44 \
                -registers $registers]]]
}

if {[info exists ::argv0] &&
    [file normalize $::argv0] eq [file normalize [info script]]} {
    set out_path [file join $script_dir histogram_statistics.svd]
    if {[llength $::argv] >= 1} {
        set out_path [lindex $::argv 0]
    }
    ::mu3e::cmsis::svd::write_device_file \
        [::mu3e::cmsis::spec::build_device] $out_path
}
