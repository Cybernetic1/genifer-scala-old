package genifer3

/*
Notes from the book "AI - a modern approach" (3rd edition).

Ch 21 talks about reinforcement learning.  The aim of Q-learning is to
learn Q values.  Each Q value Q(S,A) is the utility of doing action A in
state S.

The update rule is:
  Q(S,A) += α [ R(S) + γ max Q(S',A') - Q(S,A) ].
                          A'
where:
  α = learning rate
  γ = discount factor of rewards
  R(S) = reward gotten at state S
  S' = any state gotten from state S after action A
  the max value = U(S') = utility of S'.

The idea of Q-learning comes from TD (temporal-difference) learning,
where the update formula is:
  U(S) += α [ R(S) + γ U(S') - U(S) ].
From this we can see the 'desired' value of U(S) is:
  R(S) + γ U(S').
This update occurs whenever a state transition S -> S' occurs.

In passive learning (that is, when the policy is fixed and we just want
to find the utilities of every state), the utility U(S) is equal to its
reward plus the expected utility of all its subsequent states S':
  U(S) = R(S) + α ∑ P(S' | S, π(S)) U(S')
                  S'
where π is the fixed policy.  This is the famous Bellman optimality
condition and is also the 'equilibrium' we want to achieve in TD learning.

The TD update rule works without needing the transition probabilities
because rarely-occurring transitions are automatically accounted for.

Another method for passive learning is ADP (adaptive dynamic programming).
This involves learning the transition model P(S'|S,π(S)) and substituting
it into the Bellman condition to obtain U(S).  It simply records the
frequencies of transitions of each action, using them to estimate P(S'|S,π(S)).
It's called dynamic programming because it exploits the Bellman condition.
It uses maximum likelihood to estimate the transition model P(), and then
assumes that this is the correct model.  Choosing actions according to
such a model is not always a good idea, as we should consider the distribution
over all possible / probable models.

In active learning the policy itself has to be learned.  That brings in the
question of exploration vs exploitation, and greedy algorithms often converge
to rather bad policies because of the lack of exploration to learn models of
the world.

For active learning, an "optimistic" utility U₊ is used which includes the
"exploration function" f(utility, n):
  U₊(S) = R(S) + α max f( ∑ P(S'|S,A) U₊(S') , N(S,A) )
where N(S,A) is the number of times action A has been tried in state S.
The function f() detemines the balance of preference between utility and
novelty (represented by the number n).  f() is monotonously increasing for
utility, and monotonously decreasing for n.  A simple possible definition
of f(u,n) is:
  f(u,n)	= R		if n < N0
          = u		otherwise
which ensures that each state-action pair is tried at least N0 times.
R is a constant reward assigned to novel territories.

SARSA (state-action-reward-state-action) is a subtle variant of Q-learning
with update formula:
  Q(S,A) += α [ R(S) + γ Q(S',A') - Q(S,A) ].

Q-Learning is a model-free learning method because it does not attempt to
learn the (transition) model P(S'|S,π(S)).

When the state space gets large, functional generalization is needed to
approximate the utility function U().  U(S) could be expressed as a linear
combination of features:
  U(x,y,...) = θ0 + θ1 x + θ2 y + ...
but we can also include arbitrary non-linear functions as features.
The new update rules for utility is:
  θ += α [ R(S) + γ U(S') - U(S) ] ∂U(S)/∂θ.
For Q-values:
  θ += α [ R(S) + γ max Q(S',A') - Q(S,A) ] ∂Q(S,A)/∂θ.
However, if the parametric form is non-linear, AIMA claims that TD learning
and Q-learning based on these updates rules is no longer guaranteed to
converge.

Finally there is the method of direct policy search, which tries to search in
the space of parameters.  It acts according to:
  π(S) = arg max Q(S,A)
where the state S can be expressed as a tuple of features X = (x1, x2, ...)
and the Q value is expressed as a linear combination:
  ∑ θ X + ∑ θ A.
When the parameters change infinitesimally, the policy may change discontinously.
This makes gradient-based search difficult.  A remedy is to use a stochastic
distribution over policies, specifying the probability of choosing action A
in state S.  An example is the softmax function:
  P(A=a) = exp Q(S,A) / ∑ exp Q(S,A').
                        A'
This is a probability distribution over the random variable A, with the property
that if Q(A=a0) is higher than other Q(a) values, the probability P(A=a0) would
be disproportionately close to 1.

The policy can be chosen simply by sampling the distribution:
  π(S) = P(A).

Ch 17 talks about Value Iteration and Policy Iteration, and POMDP.

Ch 15 talks about filtering, HMM, Kalman filtering, dynamic Bayes nets.

*/

class ReinforcementLearning {

  // Q values, a map from (State,Action) to Utility:
  val Q = new scala.collection.mutable.HashMap[(Int, Int), Float]()

  var ε: Float = 0.1f          // probability to perform random action
  var α: Float = 0.2f          // Learning rate
  var γ: Float = 0.9f          // Discount rate for utilities
  var actions: Seq[Int] = null

  val rand = new util.Random

  def getQ(state: Int, action: Int) = {
    Q.getOrElse((state, action), 0.0f)     // 1.0f
  }
  
  def learnQ(state: Int, action: Int, reward: Float, value: Float) = {
    if (Q.contains(state, action)) {
      val oldv = Q(state, action)
      Q((state, action)) = oldv + α * (value - oldv)
    }
    else
      Q((state, action)) = reward
  }

  def chooseAction(state: Int): Int = {

    var action: Int = 0

    if (rand.nextFloat < ε)
      action = actions(rand.nextInt(actions.length))
    else {
      var maxQ = 0.0f
      for (a <- actions)
        if (Q(state, a) > maxQ)
        {
          maxQ = Q(state, a)
          action = a
        }

      val count = Q.count((x : ((Int, Int), Float)) => x._2 == maxQ)

      if (count > 1) {
        println("ERROR in reinforcement learning: two action Q values clashed.\n")
        // best =[i for i in range(len(actions)) if Q[i] == maxQ]
        // i = random.choice(best)
      }
    }
    return action
  }

  def learn(state1: Int, action1: Int, reward: Float, state2: Int) = {
    var maxQnew = 0.0f
    for (a <- actions)
      if (Q(state2, a) > maxQnew)
        maxQnew = Q(state2, a)

    learnQ(state1, action1, reward, reward + γ * maxQnew)
  }

  def printQ() = {
    val keys = Q.keys()

    val states = list(set([a for a, b in keys] ) )
    val actions = list(set([b for a, b in keys] ) )

    val dstates =["".join([str(int(t)) for (t <- list(tup))] ) for (tup <- states)]
    print(" " * 4) + " ".join([ "%8s" , ("(" + s + ")") for (s <- dstates)] )
    for (a <- actions)
      print ("%3d " , a) + " ".join([ "%8.2f" , (Q(s, a)) for (s <- states)] )
  }

  def printV() = {
    val keys = Q.keys()

    val states =[a for a, b <- keys]
    val statesX = list(set([x for x, y <- states] ) )
    val statesY = list(set([y for x, y <- states] ) )

    print(" " * 4) + " ".join([ "%4d" % (s) for s <- statesX] )
    for (y <- statesY)
      maxQ =[max([Q((x, y), a) for (a <- actions)] )
    for (x <- statesX)
      print("%3d ", y) + " ".join([ff(q, 4) for (q <- maxQ)] )
  }

  /*
  def ff(f, n):
  fs = "{:f}".format(f)

  if len(fs) < n:
  return ("{:" + n + "s}").format(fs)
  else:
  return fs[: n]
  */
}

/*
 s = -1 if f < 0 else 1
 ss = "-" if s < 0 else ""
 b = math.floor(math.log10(s*f)) + 1
 if b >= n:
     return ("{:" + n + "d}").format(math.round(f))
 elif b <= 0:
     return (ss + ".{:" + (n-1) + "d}").format(math.round(f * 10**(n-1)))
 else:
     return ("{:"+b+"d}.{:"+(n-b-1)+"
*/
