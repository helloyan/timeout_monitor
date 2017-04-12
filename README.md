timeout_monitor
=====

由于OTP应用中的timeout在任意事件触发后起始时间均被清零，无法针对特定事件进行超时跟踪，因此使用单独的应用来解决该问题

使用
----
1. 启动application
2. timeout_monitor:start_monitor(Pid, Type, Interval),
3. 被监控进程等待接受 {Type,Count,Pid}消息并进行处理，Pid为监控进程，可免于被监控进程需存储监控进程id的问题
4. 如果想重置超时次数，发送{reset, Pid, Type}给监控进程
5. 如果想停止监控，timeout_monitor:stop_monitor(Pid),Pid为监控进程