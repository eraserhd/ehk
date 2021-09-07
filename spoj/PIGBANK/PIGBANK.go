package main

import (
	"fmt"
	"sort"
)

const (
        MaximumWeight = 10000
        MaximumCoins = 500
)

type coin struct {
	weight int
	value  int
}

type memoCell struct {
        possible bool
        value int64
}

type PigBank struct {
	emptyWeight int
	weight      int
	coins       []coin
	memo        [MaximumWeight + 1]memoCell
}

func (bank *PigBank) ReadCase() {
	var N int
	var coins [MaximumCoins]coin
	fmt.Scanf("%d %d\n%d\n", &bank.emptyWeight, &bank.weight, &N)
	for n := 0; n < N; n++ {
		fmt.Scanf("%d %d\n", &coins[n].value, &coins[n].weight)
	}
	bank.coins = coins[0:N]
}

func (bank *PigBank) CoinWeight() int {
	return bank.weight - bank.emptyWeight
}

type ByWeightThenValue []coin

func (coins ByWeightThenValue) Len() int {
	return len(coins)
}
func (coins ByWeightThenValue) Swap(i, j int) {
	coins[i], coins[j] = coins[j], coins[i]
}
func (coins ByWeightThenValue) Less(i, j int) bool {
	switch {
	case coins[i].weight < coins[j].weight:
		return true
	case coins[i].weight > coins[j].weight:
		return false
	default:
		return coins[i].value < coins[j].value
	}
}

func (bank *PigBank) SortCoins() {
	sort.Sort(ByWeightThenValue(bank.coins))
	n := 1
	for i := 1; i < len(bank.coins); i++ {
		if bank.coins[i].weight != bank.coins[n-1].weight {
			n++
		}
	}
	bank.coins = bank.coins[0:n]
}

func (bank *PigBank) MinimumMoney() (minimum int64, possible bool) {
        bank.memo[0] = memoCell{possible: true, value: 0}
	for i := 0; i <= bank.CoinWeight(); i++ {
		for j := range bank.coins {
			if i+bank.coins[j].weight >= 10001 {
				break
			}
			if !bank.memo[i].possible {
				continue
			}
			newValue := bank.memo[i].value + int64(bank.coins[j].value)
			memop := &bank.memo[i+bank.coins[j].weight]
			if !memop.possible || memop.value > newValue {
        			*memop = memoCell{possible: true, value: newValue}
			}
		}
	}
	result := bank.memo[bank.CoinWeight()]
	return result.value, result.possible
}

func SolveCase() {
	var bank PigBank
	bank.ReadCase()
	bank.SortCoins()
	minimum, possible := bank.MinimumMoney()
	if !possible {
		fmt.Printf("This is impossible.\n")
		return
	}
	fmt.Printf("The minimum amount of money in the piggy-bank is %d.\n", minimum)
}

func main() {
	var T int
	fmt.Scanf("%d", &T)
	for t := 0; t < T; t++ {
        	SolveCase()
	}
}
