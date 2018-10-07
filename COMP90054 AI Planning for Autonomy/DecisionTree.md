## Decision Tree and Random Forest

## Decision Tree
### 1. Definition
+ A flow-chart-like tree structure

+ there are three nodes in in the decision tree:
	- root node
	- internal node
	- leaf node(also called terminal node)

+ Internal node denotes a test on an attribute
+ Branch represents an outcome of the test
+ Leaf nodes represent class labels or class distribution

### 2. Advantage and Disadvantage
+ Advantages:
	- Basic classification model
	- Fast
	- Scalable
	- Interpretable

+ Disadvantage:
	- Not highest accuracy

### 3. How to Build the Decision Tree?
+ Hunt algorithm 
	- a foudation algorithm for many other DTAs
	- a **suboptimal** decision tree
	- a **greedy** algorithm

+ General Procedure
	- Let Dt be the set of training records that reach a node t

	- 1) If Dt contains records that belong **the same class** yt
		- then t is a leaf node labeled as yt

	- 2) If Dt contains records that belong to **more than one class**, 
		- select an attribute(**attribute test condition**) test to split the data into smaller subsets. 
		- Recursively apply the procedure to each subset.
 
	- 3) If Dt is an **empty set**, 
		- then t is a leaf node labeled by the default class, yd (majority class in the data)	

+ Stopping Condition
	- 1) all records belong to the same class
	- 2) empty set


### 3. Decision Boundary 
+ Border line of between two neighbouring regions of different classes is
known as decision boundary
+ Decision boundary in decision trees is parallel to axes because test
condition involves single attribute at-a-time

+ Pure is the goal
	- then label each rectangle with one class

### Tree Induction Issues
+ Determine how to split the records
	- How to specify the attribute test condition?
	- How to determine the best split?
+ Determine when to stop splitting

### 4. How to specify the attribute test condition?

+ Dependes on attribute types
	- (binary)
	- Nomial
	- Ordinal 
	- Continuous
+ Depends on number of ways to split
	- binary split
		- Divides values into two subsets. 
		- Need to find optimal partitioning.
	- Multi-way split
		- Use as many partitions as distinct values. 

+ NOTE:In Ordinal Attributes:
	- **Do not violate the order of ordinal attributes**
	- ie. choose {small, medium}; {large , xl}
		- rather than {small large}{medium, xl}

+ For Continuous attributes
	- **Discretization** to form an ordinal categorical attribute 
		- Static	
			- discretize once at the beginning
			- ie. H1,H2,H3,Pass
		- Dynamic
			- ranges can be found by equal interval(/freq/clustering) bucketing
			- ie. 20 students in each group(H1...)
	- Binary Decision
		- (A < v) or (A >= v)
		- need to **consider all possible ranges**

### 5. How to determine the best split?
+ Again , based on **Impurity**

+ Measures of Node Impurity
	- Misclassification Error
	- Entropy
	- Gini index

#### For example:
+ |Node N1|Count|
  |---|---|
  |C=0|0|
  |C=1|6|

+ all zero

+ |Node N1|Count|
  |---|---|
  |C=0|1|
  |C=1|5|

+ Gini/Entropy/Error: 0.278/0.650/0.167

+ |Node N1|Count|
  |---|---|
  |C=0|3|
  |C=1|3|

+ Gini/Entropy/Error: 0.5/1/0.5

#### Split Measures
+ delta = Impurity of parent node - Impurity of the children node
+ larger is better

+ delta = I(p) - sum(N(vj)/N\*I(vj))
+ delta becomes information gain if use entrpy as the Impurity measure
	- the Information gain == the mutual information between  
	the class variable and test attribute
+ **Note**:
	- MI can be biased to the features with high number of values
	- when we have high # of values for one feature(usually),
		we get lower error, due to data is distributed between larger # of subsets
		hence, overall error will be lower
+ For conquering the biase problem
	- **GAIN RATIO = GAIN(split) / SplitInfo**
	- **SplitInfo** adjusts the information gain
		- higher entropy partitioning will be penalized


#### AVOID OVER-FITTING

#### Parameters
+ total number of nodes
+ tree depth
+ minimum # of data points for a split


+ cross validation

#### Random Forest
+ lots of trees 
	- voting
+ Each tree is built on a random subset of records of the data
	- **Tree bagging**
		- random subset of records of the data
	- **Random subspace**
		- random subset of features of the data

