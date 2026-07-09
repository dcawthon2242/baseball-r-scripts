#include <Rcpp.h>
#include <RcppParallel.h>
#include <cmath>

using namespace Rcpp;
using namespace RcppParallel;

// Worker for parallel distance computation
struct TrajectoryDistanceWorker : public Worker {
  // Input data
  const RVector<double> x1, y1, z1, x0, y0, z0;
  const double reaction_time;
  
  // Output data
  double total_distance;
  
  // Constructor
  TrajectoryDistanceWorker(const NumericVector x1, const NumericVector y1, const NumericVector z1,
                           const NumericVector x0, const NumericVector y0, const NumericVector z0,
                           double reaction_time)
    : x1(x1), y1(y1), z1(z1), x0(x0), y0(y0), z0(z0), reaction_time(reaction_time), total_distance(0.0) {}
  
  // Parallel operator
  void operator()(std::size_t begin, std::size_t end) {
    double delta_time = reaction_time / (end - begin);
    double dist = 0.0;
    for (std::size_t i = begin; i < end; ++i) {
      double dx = x1[i] - x0[i];
      double dy = y1[i] - y0[i];
      double dz = z1[i] - z0[i];
      dist += std::sqrt(dx * dx + dy * dy + dz * dz) * delta_time;
    }
    total_distance += dist;
  }
  
  // Join function for parallel reduction
  void join(const TrajectoryDistanceWorker& rhs) {
    total_distance += rhs.total_distance;
  }
};

// [[Rcpp::export]]
double trajectory_distance_parallel(NumericVector x1, NumericVector y1, NumericVector z1,
                                    NumericVector x0, NumericVector y0, NumericVector z0,
                                    double reaction_time) {
  // Determine the length of the shorter trajectory
  std::size_t len = std::min({x1.size(), x0.size()});
  
  // Create the worker
  TrajectoryDistanceWorker worker(x1, y1, z1, x0, y0, z0, reaction_time);
  
  // Execute parallel reduction
  parallelReduce(0, len, worker);
  
  return worker.total_distance;
}
