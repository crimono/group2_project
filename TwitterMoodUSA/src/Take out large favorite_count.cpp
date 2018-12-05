#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector fav_limit(NumericVector twitter_data_filtered) {

  NumericVector data(twitter_data_filtered.length();

  double mean_fav = mean(twitter_data_filtered);
  double sd_fav = 3*sd(twitter_data_filtered);
  double limit =  mean_fav + sd_fav;

  for(int i(1); i<=twitter_data_filtered.length(); i++)
  {
    if (limit < 5)
    {
      data(i - 1) = twitter_data_filtered(i);
    }
    else if (twitter_data_filtered(i)) >= limit)
    {
      twitter_data_filtered(i)) = limit;
      data(i - 1) = twitter_data_filtered(i);
    }
    else
    {
      data(i - 1) = twitter_data_filtered(i);
    }
  }

  return data;
}
