### Here, we test the gradient code. 

roll.dice=roll.dice.cl()
roll = roll.dice()

agent = make_vanilla_agent(40,relu,drelu,n.in=28)
old_board = random.board()

new_board = agent$move(x,roll,agent)
reward = fwd.prop(new_board,agent$weights)$a2

output = fwd.prop(old_board, agent$weights)
bprop = bk.prop(old_board,reward,agent$weights$w2,output,drelu)

out = num.gradient(old_board,reward,agent,relu,sigmoid,cost.squared.error,eps=1e-8)

bprop$db1 / as.vector(out$db1)
bprop$db2 / as.vector(out$db2)
bprop$dw1 / out$dw1
bprop$dw2 / out$dw2

# These are currently all off by a factor of 4. 
