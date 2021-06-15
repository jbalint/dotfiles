#!/bin/bash
PRICE=`curl -s -o - https://sochain.com/api/v2/get_price/DOGE/USD | jq -r '.data.prices[0].price'`
awk "BEGIN { print(int($PRICE * 23821), int($PRICE*100)/100) }"

