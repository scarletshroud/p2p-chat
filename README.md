p2p-chat
=====
## Реализация p2p-чата с использованием шифрования для передачи сообщений


#### Дисциплина: "Функциональное программирование"
#### Выполнил: Студент Зиновичев Е.С., группа P34112

Вариант использования в Eshell:
```
1> discovery_server:start_link().
{ok,<0.82.0>}

2> mpeer:join().                 
Enter your username
Ivan
To connect to the client use the following PID: <0.86.0>
ok

3> mpeer:join().
Enter your username
Egor
To connect to the client use the following PID: <0.89.0>
ok

4> mpeer:join().
Enter your username
Dmitriy
To connect to the client use the following PID: <0.92.0>
ok

5> mpeer:join().
Enter your username
Alex
To connect to the client use the following PID: <0.95.0>
ok

6> client:send_message(<0.95.0>, "Hello, I'm Alex!").
ok

Receiver Pid: <0.91.0>
Sender: Alex
Message: Hello, I'm Alex!

Receiver Pid: <0.88.0>
Sender: Alex
Message: Hello, I'm Alex!

Receiver Pid: <0.85.0>
Sender: Alex
Message: Hello, I'm Alex!

7> client:send_message(<0.89.0>, "Privet!"). 
ok

Receiver Pid: <0.94.0>
Sender: Egor
Message: Privet!

Receiver Pid: <0.91.0>
Sender: Egor
Message: Privet!

Receiver Pid: <0.85.0>
Sender: Egor
Message: Privet!
```
