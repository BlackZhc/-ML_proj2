//
//  main.cpp
//  Assignment7_practice
//
//  Created by Haochi Zhang on 11/11/21.
//
#include <iostream>
#include <iomanip>
#include <cmath>
#include <vector>
#include <fstream>
#include <cstdlib>
#include <random>
#include <chrono>
#include "/Users/haochizhang/Desktop/AU21/IE523/Code/newmat11/newmatap.h"
#include "/Users/haochizhang/Desktop/AU21/IE523/Code/newmat11/newmat.h"
#include "/Users/haochizhang/Desktop/AU21/IE523/Code/newmat11/newmatio.h"

using namespace std;
unsigned seed = (unsigned) std::chrono::system_clock::now().time_since_epoch().count();
default_random_engine generator;


double get_uniform(){
    std::uniform_real_distribution<double> distribution(-5.0,5.0);
    double number = distribution(generator);
    return number;
}



Matrix brute_force(Matrix A, int k, int n)
{
    IdentityMatrix I(n);
    Matrix B = I;
    for (int i = 0; i < k; i++){
        B = B*A;
    }
    return B;
}

Matrix repeated_squaring(Matrix A, int k, int n)
    {
        if (k == 1){
            return A;
        }
        if (k == 0){
            IdentityMatrix I(n);
            return I;
        }
        if (k%2 != 0){
            return A * repeated_squaring(A*A, (k-1)/2, n);
        }
        else
        {
            return repeated_squaring(A*A, k/2, n);
        }
    }

void display_matrix(Matrix A, int n){
    cout<<"-------------------------------------------------------------------"<<endl;
    for (int i = 0; i < n; i++){
        for (int j = 0; j < n; j++){
            cout<<setw(9)<<setprecision(3)<<A(i+1, j+1)<<"  ";
        }
        cout<<endl;
    }
    cout<<endl;
    cout<<"-------------------------------------------------------------------"<<endl;
}


Matrix create_random_matrix(Matrix A, int n){
    for (int i = 0; i < n; i++){
        for (int j = 0; j < n; j++){
            A(i+1, j+1) = get_uniform();
        }
    }
    return A;
}


bool check_result(Matrix A, int n){
    bool check;
    for (int i = 0; i < n; i++){
        for (int j = 0; j < n; j++){
            if (isnan(A(1,1)))
                //cout<<"This matrix has value too large to display"<<endl;
            return false;
            
        }
    }
    return true;
}

float count_time(Matrix A, int k, int n){
    float time_before = clock();
    Matrix B = brute_force(A, k, n);
    float time_after = clock();
    float time = (time_after - time_before)/CLOCKS_PER_SEC;
    return time;
}




int main(int argc, const char * argv[])
    {
    // insert code here...
    int n ;
    int k ;
    
    sscanf(argv[1], "%d", &n);
    sscanf(argv[2], "%d", &k);
    
    Matrix A(n, n);
    Matrix B = create_random_matrix(A, n);
    
    
    float time_before = clock();
    Matrix C = brute_force(B, k, n);
    float time_after = clock();
    float time_brute = (time_after - time_before)/CLOCKS_PER_SEC;
    
    float time_before1 = clock();
    Matrix D = repeated_squaring(B, k, n);
    float time_after1 = clock();
    float time_repeated = (time_after1 - time_before1)/CLOCKS_PER_SEC;

    

    cout<<"The number of rows/columns in the square matrix is: "<<n<<endl;
    cout<<"The exponent is: "<<k<<endl;
    cout<<"Repeated Squaring Result: "<<endl;
    
    if (n > 100)
        cout<<"This matrix has larger than 100 rows/columns"<<endl;
    else{
        if (check_result(D, 5))
            display_matrix(D, n);
        else
            cout<<"This matrix has value too large to display"<<endl;
    }
    cout<<"It took "<<time_repeated<<" seconds to complete"<<endl;
    
    cout<<endl;
    
    cout<<"Direct Multiplication Result: "<<endl;
    if (n > 100)
        cout<<"This matrix has larger than 100 rows/columns"<<endl;
    else{
        if (check_result(D, 5))
            display_matrix(C, n);
        else
            cout<<"This matrix has value too large to display"<<endl;
    }
    cout<<"It took "<<time_brute<<" seconds to complete"<<endl;
    

    
    
    
    return 0;
}
