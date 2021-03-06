%% NetFlow header Version 5
-define(NF_V5_HEADER_FORMAT,
    Version:16,
    Count:16,
    SysUptime:32,
    UnixSecs:32,
    UnixNsecs:32,
    FlowSequence:32,
    EngineType:8,
    EngineID:8,
    SamplingInterval:16
).

-record(nfh_v5, {
        version,
        count,
        sys_uptime,
        unix_secs,
        unix_nsecs,
        flow_seq,
        engine_type,
        engine_id,
        sampling_interval
    }).

%% NetFlow record Version 5
-define(NF_V5_RECORD_FORMAT,
    SrcAddr:32,
    DstAddr:32,
    NextHop:32,
    Input:16,
    Output:16,
    Pkts:32,
    Octets:32,
    First:32,
    Last:32,
    SrcPort:16,
    DstPort:16,
    _Pad1:8,
    TcpFlags:8,
    Prot:8,
    Tos:8,
    SrcAs:16,
    DstAs:16,
    SrcMask:8,
    DstMask:8,
    _Pad2:16
).

-record(nfrec_v5, {
        src_addr,
        dst_addr,
        next_hop,
        input,
        output,
        d_pkts,
        d_octets,
        first,
        last,
        src_port,
        dst_port,
        tcp_flags,
        prot,
        tos,
        src_as,
        dst_as,
        src_mask,
        dst_mask
    }).
