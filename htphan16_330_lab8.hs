import Data.List
-- Coding Assignment 8.1: (2 Points)

data MyBinaryTree a = NullNode | Node a (MyBinaryTree a) (MyBinaryTree a)  
                    deriving (Show, Eq)

-- Coding Assignment 8.2: (2 Points)

leftTree :: Eq a => (MyBinaryTree a -> MyBinaryTree a)
leftTree NullNode = NullNode
leftTree (Node a l r)
  | (Node a l r == Node a NullNode r) = NullNode
  | otherwise = l

rightTree :: Eq a => (MyBinaryTree a -> MyBinaryTree a)
rightTree NullNode = NullNode
rightTree (Node a l r)
  | (Node a l r == Node a l NullNode) = NullNode
  | otherwise = r

-- Coding Assignment 8.3: (2 Points)

treeElem :: Eq a => (a -> MyBinaryTree a -> Bool)
treeElem x NullNode = False
treeElem x (Node a l r)
  | (x == a) = True
  | (l /= NullNode) = treeElem x l
  | (r /= NullNode) = treeElem x r
  | otherwise = False

-- Coding Assignment 8.4: (4 Points)

treeMax :: Ord a => (MyBinaryTree a -> a)
treeMax NullNode = error "Binary tree has no elements"
treeMax (Node a l r)
  | (Node a l r == Node a NullNode NullNode) = a
  | (Node a l r == Node a NullNode r) = max y a
  | (Node a l r == Node a l NullNode) = max x a
  | otherwise = maximum [x, y, a]
    where x = treeMax l
          y = treeMax r

treeMin :: Ord a => (MyBinaryTree a -> a)
treeMin NullNode = error "Binary tree has no elements"
treeMin (Node a l r)
  | (Node a l r == Node a NullNode NullNode) = a
  | (Node a l r == Node a NullNode r) = min y a
  | (Node a l r == Node a l NullNode) = min x a
  | otherwise = minimum [x, y, a]
    where x = treeMin l
          y = treeMin r

-- Coding Assignment 8.5: (4 Points)

reflectTree :: Eq a => (MyBinaryTree a -> MyBinaryTree a)
reflectTree NullNode = NullNode
reflectTree (Node a l r) 
  | (Node a l r == Node a NullNode r) = (Node a r NullNode)
  | (Node a l r == Node a l NullNode) = (Node a NullNode l)
  | otherwise = (Node a (reflectTree r) (reflectTree l))

-- Coding Assignment 8.5: (4 Points)

collapseTree :: Eq a => (MyBinaryTree a -> [a])
collapseTree NullNode = []
collapseTree (Node a l r)
  | (Node a l r == Node a NullNode NullNode) = [a]
  | (Node a l r == Node a l NullNode) = collapseTree l ++ [a]
  | (Node a l r == Node a NullNode r) = [a] ++ collapseTree r
  | otherwise = collapseTree l ++ [a] ++ collapseTree r

-- Coding Assignment 8.6: (4 Points)

checkSortedList :: Ord a => ([a] -> Bool)
checkSortedList [] = error "List is empty"
checkSortedList [x] = True
checkSortedList (x:y:xs)
  | (x <= y) = checkSortedList (y:xs) 
  | otherwise = False

isBST :: Ord a => (MyBinaryTree a -> Bool)
isBST NullNode = False
isBST x = checkSortedList $ collapseTree x

-- Coding Assignment 8.7: (4 Points)

bstAdd :: Ord a => (a -> MyBinaryTree a -> MyBinaryTree a)
bstAdd x NullNode = Node x NullNode NullNode
bstAdd x (Node a l r)
  | (isBST (Node a l r) == False) = error "Input is not Binary Search Tree" 
  | ((Node a l r == Node a NullNode NullNode) && (x <= a)) = Node a (Node x NullNode NullNode) NullNode
  | ((Node a l r == Node a NullNode NullNode) && (x >= a)) = Node a NullNode (Node x NullNode NullNode)
  | (isBST (Node a l (bstAdd x r))) = Node a l (bstAdd x r)
  | otherwise = Node a (bstAdd x l) r

-- Coding Assignment 8.8: (4 Points)

listToBST :: Ord a => ([a] -> MyBinaryTree a)
listToBST [] = NullNode
listToBST [x] = Node x NullNode NullNode
listToBST l = bstAdd (last l) (listToBST (init l))

treeToBST :: Ord a => (MyBinaryTree a -> MyBinaryTree a)
treeToBST x = listToBST $ collapseTree x

-- Coding Assignment 8.9: (6 Points)

delFromBST :: Ord a => (a -> MyBinaryTree a -> MyBinaryTree a)
delFromBST x NullNode = NullNode
delFromBST x (Node a l r)
  | (isBST (Node a l r) == False) = error "Input is not Binary Search Tree"
  | (treeElem x (Node a l r) == False) = Node a l r 
  | otherwise = listToBST $ delete x $ collapseTree (Node a l r)

--Coding Assignment 8.10: (4 Points)

binaryLookup :: Ord a => (a -> MyBinaryTree a -> Bool)
binaryLookup x NullNode = False
binaryLookup x (Node a l r)
  | (x == a) = True
  | (isBST (Node a l r) == False) = binaryLookup x (treeToBST (Node a l r))
  | (x < a) = binaryLookup x l 
  | (x > a) = binaryLookup x r

-- Coding Assignment 8.11: (10 Points)

-- Create an algebraic type for a queue, called MyQueue

data MyQueue a = EmptyQueue | QueueNode a (MyQueue a)
               deriving (Show, Eq)

-- Functions that facilitate basic operations with queue

-- Function queueElem tests whether a queue contains a value
-- Takes 2 arguments a and MyQueue a and returns a True if a value is in the queue

queueElem :: Eq a => (a -> MyQueue a -> Bool)
queueElem x EmptyQueue = False
queueElem x (QueueNode a q) 
  | (x == a) = True
  | otherwise = queueElem x q

-- Function enqueue adds a value to the end of an existing queue
-- Takes 2 arguments a and MyQueue a and returns a queue with the new value added

enqueue :: Eq a => (a -> MyQueue a -> MyQueue a)
enqueue x EmptyQueue = QueueNode x EmptyQueue
enqueue x (QueueNode a q)
  | (q == EmptyQueue) = QueueNode a (QueueNode x EmptyQueue)
  | otherwise = enqueue x q

-- Function dequeue removes the first value of an existing queue
-- Takes MyQueue a as an argument and returns a tuple containing the new queue and the value that is removed from the original queue

dequeue :: Eq a => (MyQueue a -> (MyQueue a, a))
dequeue EmptyQueue = error "Queue is empty"
dequeue (QueueNode a q) = (q, a)

-- Functions that use queue for something

-- Function queueToList converts a queue into a list
-- Takes MyQueue a as an argument and returns a list with elements of the queue in its queue order, or an empty list if queue is empty

queueToList :: Eq a => (MyQueue a -> [a])
queueToList EmptyQueue = []
queueToList (QueueNode a q)
  | (q == EmptyQueue) = [a]
  | otherwise = [a] ++ queueToList q

-- Function listToQueue converts a list into a queue
-- Takes a list as an argument and returns a queue with elements of the list in its list order, or an empty queue if list is empty

listToQueue :: Eq a => ([a] -> MyQueue a)
listToQueue [] = EmptyQueue
listToQueue [x] = QueueNode x EmptyQueue
listToQueue l = enqueue (last l) (listToQueue (init l))

-- Function queueMin, queueMax find the smallest and greatest element in the queue
-- Takes MyQueue a as an argument and returns the smallest element (queueMin) or the greatest element (queueMax) in the queue

queueMin :: Ord a => (MyQueue a -> a)
queueMin EmptyQueue = error "Queue is empty"
queueMin x = minimum (queueToList x)

queueMax :: Ord a => (MyQueue a -> a)
queueMax EmptyQueue = error "Queue is empty"
queueMax x = maximum (queueToList x)

-- Function delFromQueue deletes an element in the queue
-- Takes a value and MyQueue a as arguments and returns the new queue with the value deleted from the queue

delFromQueue :: Eq a => (a -> MyQueue a -> MyQueue a)
delFromQueue x EmptyQueue = EmptyQueue
delFromQueue x (QueueNode a q)
  | (queueElem x (QueueNode a q)) = listToQueue $ delete x $ queueToList (QueueNode a q)
  | otherwise = QueueNode a q

-- Function delMinFromQueue, delMaxFromQueue delete the smallest element and greatest element in the queue
-- Takes MyQueue a as an argument and returns the new queue with the smallest element (delMinFromQueue) or the greatest element (delMaxFromQueue) deleted from the queue

delMinFromQueue :: Ord a => (MyQueue a -> MyQueue a)
delMinFromQueue EmptyQueue = EmptyQueue
delMinFromQueue (QueueNode a q) = delFromQueue (queueMin (QueueNode a q)) (QueueNode a q) 

delMaxFromQueue :: Ord a => (MyQueue a -> MyQueue a)
delMaxFromQueue EmptyQueue = EmptyQueue
delMaxFromQueue (QueueNode a q) = delFromQueue (queueMax (QueueNode a q)) (QueueNode a q) 
