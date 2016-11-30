import tensorflow as tf
import numpy as np

xy = np.loadtxt('07train.txt', unpack=True)
x_data = np.transpose(xy[0:-1])
y_data = np.reshape(xy[-1], (4, 1))

# Neural Network 1 For XOR Deep & Wide & Tensorboard

print x_data
print y_data

X = tf.placeholder(tf.float32, name='X-input')
Y = tf.placeholder(tf.float32, name='Y-input')

W1 = tf.Variable(tf.random_uniform([2, 2], -1.0, 1.0), name='Weight1')
W2 = tf.Variable(tf.random_uniform([2, 1], -1.0, 1.0), name='Weight2')

b1 = tf.Variable(tf.zeros([2]), name="Bias1")
b2 = tf.Variable(tf.zeros([1]), name="Bias2")

# Our hypothesis
with tf.name_scope("layer2") as scope:
	L2 = tf.sigmoid(tf.matmul(X, W1) + b1)

with tf.name_scope("layer2") as scope:
	hypothesis = tf.sigmoid(tf.matmul(L2, W2) + b2)

# cost function
with tf.name_scope('cost') as scope:
    cost = -tf.reduce_mean(Y * tf.log(hypothesis) + (1-Y) * tf.log(1 - hypothesis))
    cost_summ = tf.scalar_summary("cost", cost)

# Minimize
with tf.name_scope('train') as scope:
    a = tf.Variable(0.1) # Learning rate, alpha
    optimizer = tf.train.GradientDescentOptimizer(a)
    train = optimizer.minimize(cost)

w1_hist = tf.histogram_summary("weights1", W1)
w2_hist = tf.histogram_summary("weights2", W2)

b1_hist = tf.histogram_summary("biases1", b1)
b2_hist = tf.histogram_summary("biases2", b2)

y_hist = tf.histogram_summary("y", Y)

# Before starting, initialize the variables. We will 'run' this first.
init = tf.initialize_all_variables()

# Launch the graph.
with tf.Session() as sess:
    sess.run(init)

    merged = tf.merge_all_summaries()
    writer = tf.train.SummaryWriter("./logs/xor_logs", sess.graph_def)

    # Fit the line
    #for step in xrange(200000):
    #    summary, _ = sess.run([merge, train], feed_dict={X: x_data, Y: y_data})
    #    writer.add_summary(summary, step)
    for step in xrange(20000):
        sess.run(train, feed_dict={X: x_data, Y: y_data})
        if step % 200 == 0:
            summary = sess.run(merged, feed_dict={X: x_data, Y: y_data})
            writer.add_summary(summary, step)
            print step, sess.run(cost, feed_dict={X: x_data, Y: y_data}), sess.run(W1), sess.run(W2)

    # Test model
    correct_prediction = tf.equal(tf.floor(hypothesis+0.5), Y)
	# Calculate accuracy
    accuracy = tf.reduce_mean(tf.cast(correct_prediction, "float"))
    print sess.run([hypothesis, tf.floor(hypothesis+0.5), correct_prediction], feed_dict={X: x_data, Y: y_data})
    print "Accuracy", accuracy.eval({X: x_data, Y: y_data})
	
# $ tensorboard --logdir=./logs/xor_logs
