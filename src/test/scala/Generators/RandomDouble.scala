package org.vricsa
package Generators

import breeze.linalg.{DenseMatrix, DenseVector}

object RandomMatrixVectorGenerator extends App {

  // Method to generate a random DenseVector
  def generateRandomVector(length: Int): DenseVector[Double] = {
    val const: Double = 0.1
    DenseVector.rand[Double](length) * const
  }

  // Method to generate a random DenseMatrix
  def generateRandomMatrix(rows: Int, cols: Int): DenseMatrix[Double] = {
    val const: Double = 0.1
    DenseMatrix.rand[Double](rows, cols) * const
  }

  
}
