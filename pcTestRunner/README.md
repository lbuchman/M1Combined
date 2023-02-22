# teensyControl


mkdir ~/.teensy
cp config.json ~/.teensy

curl --header "Content-Type: application/json" --request POST  --data '{"cmd":"motorgohome","arg":"null", "timeout": "10"}' http://127.0.0.1:3000/commands

curl --header "Content-Type: application/json" --request POST  --data '{"hexfile":"http://localhost:8080/f2fTestFixture.hex", "mcu": "TEENSY40"}' http://127.0/program

curl --header "Content-Type: application/json" --request POST  --data '{"cmd":"motormove","arg":"180", "timeout": "1000"}' http://127.0.0.1:3000/commands

curl --header "Content-Type: application/json" --request POST  --data '{"cmd":"stopautotest","arg":"null", "timeout": "10"}' http://127.0.0.1:3000/commands

curl --header "Content-Type: application/json" --request POST  --data '{"cmd":"startautotest","arg":"null", "timeout": "10"}' http://127.0.0.1:3000/commands



 bdtf printstatus  pi41t  | python -m json.tool