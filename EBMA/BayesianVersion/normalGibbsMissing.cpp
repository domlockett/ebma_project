#include <Rcpp.h>
using namespace Rcpp;



// [[Rcpp::export]]
IntegerVector oneMultinomCalt(NumericVector probs) {
  int k = probs.size();
  IntegerVector ans(k);
  rmultinom(1, probs.begin(), k, ans.begin());
  return(ans);
}

// [[Rcpp::export]]
NumericVector getRGamma(double shape) { 
  RNGScope scope;
  NumericVector x = rgamma(1, shape, 1 );
  return x;
}

// [[Rcpp::export]]
LogicalVector isNA(NumericVector x) {
  int n = x.size();
  LogicalVector out(n);
  
  for (int i = 0; i < n; ++i) {
    out[i] = NumericVector::is_na(x[i]);
  }
  return out;
}

// [[Rcpp::export]]
Rcpp::List GibbsNormalMissing(Rcpp::NumericVector outcome, Rcpp::NumericMatrix prediction, Rcpp::NumericVector W, Rcpp::NumericVector alpha, double sigma, int iterations, int burnin, int thin) {
    
    int length = prediction.nrow();
    int nmods = prediction.ncol();
    int outcount = 0;
    int output = round((iterations-burnin)/thin);
    Rcpp::List theta_post(iterations);
    Rcpp::NumericMatrix W_post(iterations,nmods);
    Rcpp::NumericMatrix W_out(output,nmods);
    Rcpp::NumericVector Sigma_post(iterations);
    Rcpp::NumericVector Sigma_out(output);
    Rcpp::NumericMatrix MissingInd(length,nmods);

    for(int i = 0; i < length; i++){;
      MissingInd(i,_) = isNA(prediction(i,_));
    }
    
    for (int iterator = 0; iterator < iterations; iterator++){;
      Rcpp::NumericVector W_use(nmods);
      Rcpp::NumericMatrix evalsEach(length,nmods);
      Rcpp::NumericVector evalsAll(length);
      Rcpp::NumericMatrix theta(length,nmods);
      Rcpp::NumericMatrix T(length,nmods);
      Rcpp::NumericVector eta(nmods);
      Rcpp::NumericMatrix ssq(length,nmods);
      double temp(length);
      double sigma_use;
      Rcpp::NumericVector w_gamma(nmods);
      
      if(iterator == 0){;
        W_use = W;
        sigma_use = sigma;
      }
      if(iterator > 0){;
        W_use = W_post(iterator-1,_);
        sigma_use = Sigma_post(iterator-1);
      }    
      for(int m = 0; m < nmods; m++){;
        for(int i = 0; i < length; i++){;
          if(MissingInd(i,m) == FALSE){;
            evalsEach(i,m) = W_use(m)*(R::dnorm(outcome(i),prediction(i,m),sigma_use,false));
            };
          if(MissingInd(i,m) == TRUE){;
            evalsEach(i,m) = 0;
            };
        };
      };
      for(int i = 0; i < length; i++){
        evalsAll(i) = sum(evalsEach(i,_));
        theta(i,_) = evalsEach(i,_)/evalsAll(i);
      }
      
      for(int i=0; i<length; i++){;
        T(i,_) = oneMultinomCalt(theta(i,_));
      };
      
      for(int m=0; m<nmods; m++){
        eta(m) = alpha(m) + sum(T(_,m));
      }  
      for(int i=0; i<length; i++){;
        for(int m = 0; m < nmods; m++){
          if(MissingInd(i,m) == FALSE){;
            ssq(i,m) = theta(i,m)*((outcome(i)-prediction(i,m))*(outcome(i)-prediction(i,m)));
          };
          if(MissingInd(i,m) == TRUE){;
            ssq(i,m) = 0;
          };
        }
      }
      temp = (sum(ssq))/2;
      
      double sample_sum = 0;
      
      for(int m = 0; m<nmods; m++){
        w_gamma(m) = as<double>(rgamma(1, eta(m), 1));
        sample_sum += w_gamma(m);
      }
      
      for(int m = 0; m<nmods; m++){
        W_post(iterator,m) =  w_gamma(m)/sample_sum;
        if(((iterator+1) % thin == 0) and (iterator+1 > burnin)){;
          W_out(outcount,m) =   W_post(iterator,m);
        }
      }
      
      Sigma_post(iterator) = sqrt(1/(::Rf_rgamma((length-1)/2,(1/temp))));
      if(((iterator+1) % thin == 0) and (iterator+1 > burnin)){;
        Sigma_out(outcount) = Sigma_post(iterator);
      }
      
      if(((iterator+1) % thin == 0) and (iterator+1 > burnin)){;
        outcount += 1;
      }  
      if ((iterator+1) % 1000 == 0 ){;
        Rcpp::Rcout << "Iteration: " << iterator+1 << std::endl;
      }
    };   
    
    
    
    
    return Rcpp::List::create(Rcpp::Named("W") = W_out, Rcpp::Named("Sigma") = Sigma_out);
}