library(tensorflow)


FEATURE_SIZE = 28L
NUM_CLASSES = 1L


f.prop <- function(features, hidden1_units, hidden2_units) {
  
  # Hidden Layer 1
  with(tf$name_scope('hidden1'), {
    weights <- tf$Variable(
      tf$truncated_normal(shape(FEATURE_SIZE, hidden1_units),
                          stddev = 1.0 / sqrt(FEATURE_SIZE)),
      name = 'weights'
    )
    biases <- tf$Variable(tf$zeros(shape(hidden1_units),
                                   name = 'biases'))
    hidden1 <- tf$nn$relu(tf$matmul(tf$reshape(features, shape(1,28)), weights) + biases)
  })
  
  # Hidden 2
  with(tf$name_scope('hidden2'), {
    weights <- tf$Variable(
      tf$truncated_normal(shape(hidden1_units, hidden2_units),
                          stddev = 1.0 / sqrt(hidden1_units)),
      name = 'weights')
    biases <- tf$Variable(tf$zeros(shape(hidden2_units)),
                          name = 'biases')
    hidden2 <- tf$nn$relu(tf$matmul(hidden1, weights) + biases)
  })
  
  # Sigmoid Regression
  with(tf$name_scope('regression'), {
    weights <- tf$Variable(
      tf$truncated_normal(shape(hidden2_units, NUM_CLASSES),
                          stddev = 1.0 / sqrt(hidden2_units)),
      name = 'weights')
    biases <- tf$Variable(tf$zeros(shape(NUM_CLASSES)),
                          name = 'biases')
    out <- tf$sigmoid(tf$matmul(hidden2, weights) + biases)
  })
  
  # return output of sigmoid layer
  out
}


sess = tf$Session()
sess$run(tf$global_variables_initializer())
sess$run(f.prop(random.board(),20,1))

