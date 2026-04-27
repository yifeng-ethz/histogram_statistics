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
            -description {Requested histogram source. The bridge applies a change only once both packet streams are idle.} \
            -access read-write \
            -fields [list \
                [::mu3e::cmsis::svd::field select_post 0 1 -description "0 selects pre-hit-stack traffic, 1 selects post-hit-stack traffic." -access read-write] \
                [::mu3e::cmsis::svd::field reserved 1 31 -description "Reserved, read as zero." -access read-only]]] \
        [::mu3e::cmsis::svd::register STATUS 0x0C \
            -description {Live selector state and packet-idle visibility for safe source switching.} \
            -access read-only \
            -fields [list \
                [::mu3e::cmsis::svd::field live_select_post 0 1 -description "Currently active histogram source. 0=pre, 1=post." -access read-only] \
                [::mu3e::cmsis::svd::field requested_select_post 1 1 -description "Last requested histogram source from CONTROL.select_post." -access read-only] \
                [::mu3e::cmsis::svd::field switch_pending 2 1 -description "1 while the requested source differs from the live source." -access read-only] \
                [::mu3e::cmsis::svd::field reserved0 3 5 -description "Reserved, read as zero." -access read-only] \
                [::mu3e::cmsis::svd::field pre_packet_active 8 1 -description "1 while the forwarded pre-hit-stack path is inside a packet." -access read-only] \
                [::mu3e::cmsis::svd::field post_packet_active 9 1 -description "1 while the forwarded post-hit-stack path is inside a packet." -access read-only] \
                [::mu3e::cmsis::svd::field reserved1 10 22 -description "Reserved, read as zero." -access read-only]]]]

    return [::mu3e::cmsis::svd::device MU3E_HISTOGRAM_INGRESS_BRIDGE \
        -version 26.0.0.0421 \
        -description "CMSIS-SVD description of the histogram ingress selector bridge CSR aperture. BaseAddress is 0 because this file describes the relative CSR aperture of the IP; system integration supplies the live slave base address." \
        -peripherals [list \
            [::mu3e::cmsis::svd::peripheral HISTOGRAM_INGRESS_BRIDGE_CSR 0x0 \
                -description "Relative 4-word CSR aperture for the histogram ingress selector bridge." \
                -groupName MU3E_HISTOGRAM_INGRESS_BRIDGE \
                -addressBlockSize 0x10 \
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
