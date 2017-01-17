# Below you will find functions that are useful in the application of ERGMs for the analysis of event graphs.
# The functions all assume that the user has a table with node properties, which at least has the following 2 columns:
# "Id" (to identify nodes/events)
# "Time" (storing the time of occurrence of nodes/events)

# The CreateConstraints function is used to create the proper constraints to be used for any ERGM model applied to event graphs.
# We assume that arcs between events indicate responses (pointing backwards in time)
# An arc from any event A to any event B (A->B) can never occur if event A happened earlier than event B. 
# We therefore identify all such cases in our data, and create a matrix that marks these pairs of events with a '1'.
# This matrix (e.g., absent.edges) can be included as a constraint in models by adding the following option:
# constraints = ~fixedas(absent = absent.edges).

CreateConstraints <-function(nodesProperties) {
    nodesLabels <- unique(nodesProperties[,"Id"])
    newMatrix <- matrix(0, nrow = length(nodesLabels), ncol = length(nodesLabels))
    rownames(newMatrix) <- nodesLabels
    colnames(newMatrix) <- nodesLabels
    for (i in 1:nrow(newMatrix)) {
        for (j in 1:ncol(newMatrix)) {
            currentRowTime <- nodesProperties[(which(nodesProperties[,"Id"] == rownames(newMatrix)[i])),"Time"]
            currentColTime <- nodesProperties[(which(nodesProperties[,"Id"] == colnames(newMatrix)[j])),"Time"]
            if (currentRowTime < currentColTime) {
                newMatrix[i, j] <- 1
            }
        }
    }
    return(newMatrix)
}

# The CalculateNodeMix function requires the user to identify a categorical variable of nodes (variableString),
# as well as two values that the categorical variable can take (sourceString and targetString). 
# The function creates a matrix for all edges where this particular mix occurs. 

CalculateNodeMix <- function(nodesProperties, variableString, sourceString, targetString) {
    res.matrix <- matrix(nrow = nrow(nodesProperties), ncol = nrow(nodesProperties))
    rownames(res.matrix) <- nodesProperties[, "Id"]
    colnames(res.matrix) <- nodesProperties[, "Id"]

    for (i in 1:nrow(nodesProperties)) {
        for (j in 1:nrow(nodesProperties)) {
            if (nodesProperties[i, variableString] == sourceString &&
                nodesProperties[j, variableString] == targetString) {
                res.matrix[i, j] <- 1
            } else {
                res.matrix[i, j] <- 0
            }
        }
    }
    return(res.matrix)
}

# The CalculateNodeMix function requires the user to identify a categorical variable of nodes (variableString),
# The function creates a matrix for all edges where the nodes have the same value for that variable. 

CalculateNodeMatch <- function(nodesProperties, variableString) {
    res.matrix <- matrix(nrow = nrow(nodesProperties), ncol = nrow(nodesProperties))
    rownames(res.matrix) <- nodesProperties[, "Id"]
    colnames(res.matrix) <- nodesProperties[, "Id"]

    for (i in 1:nrow(nodesProperties)) {
        for (j in 1:nrow(nodesProperties)) {
            if (nodesProperties[i, variableString] == nodesProperties[j, variableString]) {
                res.matrix[i, j] <- 1
            } else {
                res.matrix[i, j] <- 0
            }
        }
    }
    return(res.matrix)
}

# The CalculateDistanceTime function calculates the difference in time of occurrence of each pair of events
# in the network, and returns the result as a distance matrix.

CalculateDistanceTime <- function(nodesProperties) {
    res.matrix <- matrix(0, nrow = nrow(nodesProperties), ncol = nrow(nodesProperties))
    rownames(res.matrix) <- nodesProperties[,"Id"]
    colnames(res.matrix) <- nodesProperties[,"Id"]
    for (i in 1:nrow(nodesProperties)) {
        for (j in 1:nrow(nodesProperties)) {
            res.matrix[i, j] <- nodesProperties[i, "Time"] - nodesProperties[j, "Time"]
        }
    }
    return(res.matrix)
}
