package require Tcl 8.5

set script_dir [file dirname [info script]]
set helper_file [file normalize [file join $script_dir .. toolkits infra cmsis_svd lib mu3e_cmsis_svd.tcl]]
source $helper_file

namespace eval ::mu3e::cmsis::spec {}

proc ::mu3e::cmsis::spec::build_device {} {
    set registers [list \
        [::mu3e::cmsis::svd::register UID 0x00 \
            -description {Software-visible IP identifier. Default ASCII "HISB" (0x48495342).} \
            -access read-only \
            -resetValue 0x48495342 \
            -fields [list \
                [::mu3e::cmsis::svd::field value 0 32 \
                    -description {Compile-time or integration-time UID word. Default ASCII "HISB".} \
                    -access read-only]]] \
        [::mu3e::cmsis::svd::register META 0x04 \
            -description {Read-multiplexed metadata word. Write meta_sel[1:0] before reading back: 0=VERSION, 1=DATE, 2=GIT, 3=INSTANCE_ID.} \
            -access read-write \
            -fields [list \
                [::mu3e::cmsis::svd::field meta_sel 0 2 -description "Selects the META read page." -access read-write] \
                [::mu3e::cmsis::svd::field reserved 2 30 -description "Reserved, read as zero on selector writeback." -access read-only]]] \
        [::mu3e::cmsis::svd::register CONTROL 0x08 \
            -description {Requested histogram source and counter clear control. The bridge applies a source change once both input streams are beat-idle and the post-hit-stack frame is idle.} \
            -access read-write \
            -fields [list \
                [::mu3e::cmsis::svd::field select_post 0 1 -description "0 selects pre-hit-stack traffic, 1 selects post-hit-stack traffic." -access read-write] \
                [::mu3e::cmsis::svd::field reserved0 1 7 -description "Reserved, read as zero." -access read-only] \
                [::mu3e::cmsis::svd::field clear_counters 8 1 -description "Write 1 to clear PRE_SEEN_COUNT, POST_SEEN_COUNT, HIST_EMIT_COUNT, and HIST_DROP_COUNT." -access write-only] \
                [::mu3e::cmsis::svd::field reserved1 9 23 -description "Reserved, read as zero." -access read-only]]] \
        [::mu3e::cmsis::svd::register STATUS 0x0C \
            -description {Live selector state and idle visibility for source switching. The pre packet-active field is run-level status and does not block source switching.} \
            -access read-only \
            -fields [list \
                [::mu3e::cmsis::svd::field live_select_post 0 1 -description "Currently active histogram source. 0=pre, 1=post." -access read-only] \
                [::mu3e::cmsis::svd::field requested_select_post 1 1 -description "Last requested histogram source from CONTROL.select_post." -access read-only] \
                [::mu3e::cmsis::svd::field switch_pending 2 1 -description "1 while the requested source differs from the live source." -access read-only] \
                [::mu3e::cmsis::svd::field reserved0 3 5 -description "Reserved, read as zero." -access read-only] \
                [::mu3e::cmsis::svd::field pre_packet_active 8 1 -description "1 while the forwarded pre-hit-stack path is inside a run-level packet. This is status-only for source switching." -access read-only] \
                [::mu3e::cmsis::svd::field post_packet_active 9 1 -description "1 while the forwarded post-hit-stack path is inside a packet. This blocks source switching." -access read-only] \
                [::mu3e::cmsis::svd::field post_hit_filter_enabled 10 1 -description "1 when the post-hit-stack histogram tap forwards only real hit words after K23.7 subheaders." -access read-only] \
                [::mu3e::cmsis::svd::field post_hit_region 11 1 -description "Current post-stream parser state. 1 after a K23.7 subheader until the next frame header or trailer." -access read-only] \
                [::mu3e::cmsis::svd::field reserved1 12 20 -description "Reserved, read as zero." -access read-only]]] \
        [::mu3e::cmsis::svd::register PRE_SEEN_COUNT 0x10 \
            -description {Saturating count of pre_in accepted beats since reset or CONTROL.clear_counters.} \
            -access read-only \
            -fields [list \
                [::mu3e::cmsis::svd::field value 0 32 -description "Pre-rbCAM accepted beat count." -access read-only]]] \
        [::mu3e::cmsis::svd::register POST_SEEN_COUNT 0x14 \
            -description {Saturating count of accepted post_in hit words after the optional post hit filter since reset or CONTROL.clear_counters.} \
            -access read-only \
            -fields [list \
                [::mu3e::cmsis::svd::field value 0 32 -description "Post-rbCAM accepted hit-word count." -access read-only]]] \
        [::mu3e::cmsis::svd::register HIST_EMIT_COUNT 0x18 \
            -description {Saturating count of histogram output handshakes since reset or CONTROL.clear_counters.} \
            -access read-only \
            -fields [list \
                [::mu3e::cmsis::svd::field value 0 32 -description "Histogram output handshake count." -access read-only]]] \
        [::mu3e::cmsis::svd::register HIST_DROP_COUNT 0x1C \
            -description {Saturating count of selected histogram valid cycles stalled by hist_out.ready low since reset or CONTROL.clear_counters.} \
            -access read-only \
            -fields [list \
                [::mu3e::cmsis::svd::field value 0 32 -description "Selected histogram stall count." -access read-only]]]]

    return [::mu3e::cmsis::svd::device MU3E_HISTOGRAM_INGRESS_BRIDGE \
        -version 26.0.9.0515 \
        -description "CMSIS-SVD description of the histogram ingress selector bridge CSR aperture. BaseAddress is 0 because this file describes the relative CSR aperture of the IP; system integration supplies the live slave base address." \
        -peripherals [list \
            [::mu3e::cmsis::svd::peripheral HISTOGRAM_INGRESS_BRIDGE_CSR 0x0 \
                -description "Relative 8-word CSR aperture for the histogram ingress selector bridge." \
                -groupName MU3E_HISTOGRAM_INGRESS_BRIDGE \
                -addressBlockSize 0x20 \
                -registers $registers]]]
}

if {[info exists ::argv0] &&
    [file normalize $::argv0] eq [file normalize [info script]]} {
    set out_path [file join $script_dir histogram_ingress_bridge.svd]
    if {[llength $::argv] >= 1} {
        set out_path [lindex $::argv 0]
    }
    ::mu3e::cmsis::svd::write_device_file \
        [::mu3e::cmsis::spec::build_device] $out_path
}
