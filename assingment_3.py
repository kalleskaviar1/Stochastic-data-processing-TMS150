import numpy as np
import matplotlib.pyplot as plt

#Question 1

def f(x):
    return x**4

#Analytical solution
theta = 1/5 

def crude_mc(M, f):
    theta_estimate = 0

    for k in range(M):
        #Generate independant uniform random number
        U = np.random.rand()

        theta_estimate += f(U)

    #Compute mean value
    theta_estimate /= M 
    return theta_estimate
 
#Question 2
def hit_or_miss_mc(M,f):
    count = 0

    for k in range(M):
        # Generate two independent uniform random numbers
        X = np.random.rand()
        Y = np.random.rand()

        # Check if the point (X,Y) lies below the curve f(X)
        if Y <= f(X):
            count += 1

    # Fraction of points below the curve approximates the area
    theta_estimate = count / M
    return theta_estimate

#print(crude_mc(1000,f))
#print(hit_or_miss_mc(1000,f))


#Question 3

def compare_mc_methods(power=20, rmse_iter=20):

    M_values = [2**i for i in range(1,power+1)]
    
    errors_crude = []
    errors_hit = []
    rmse_crude = []
    rmse_hit = []

    for M in M_values:
        crude = crude_mc(M,f)
        hit_or_miss = hit_or_miss_mc(M,f)

        #Errors for both MC
        errors_crude.append(abs(crude-theta))
        errors_hit.append(abs(hit_or_miss-theta))

        #Estimates for both MC
        crude_estimates = [crude_mc(M,f) for _ in range(rmse_iter)]
        hit_or_miss_estimates = [hit_or_miss_mc(M,f) for _ in range(rmse_iter)]

        #RMSE for both MC
        rmse_crude.append(np.sqrt(np.mean([(x - theta)**2 for x in crude_estimates])))
        rmse_hit.append(np.sqrt(np.mean([(x - theta)**2 for x in hit_or_miss_estimates])))

    # Plot errors
    plt.figure(figsize=(12,5))

    # Absolute errors
    plt.subplot(1,2,1)
    plt.loglog(M_values, errors_crude, label="Crude MC Error")
    plt.loglog(M_values, errors_hit, label="Hit-or-Miss MC Error")
    plt.loglog(M_values, [1/np.sqrt(M) for M in M_values], label="M^(-1/2)")
    plt.xlabel("M")
    plt.ylabel("Error")
    plt.legend()
    plt.title("Absolute Error")

    #RMSE
    plt.subplot(1,2,2)
    plt.loglog(M_values, rmse_crude, label="Crude MC RMSE")
    plt.loglog(M_values, rmse_hit, label="Hit-or-Miss MC RMSE")
    plt.loglog(M_values, [1/np.sqrt(M) for M in M_values], 'k--', label="M^(-1/2)")
    plt.xlabel("M")
    plt.ylabel("RMSE")
    plt.legend()
    plt.title("Root Mean Squared Error")

    plt.tight_layout()
    plt.show()

#compare_mc_methods()

#Question 4


def brownian_path(T=1, i_max=10):
    #Set seed for testing and development
    np.random.seed(321)

    #Increments on fine grid
    N_fine = 2**i_max
    h_fine = T/N_fine
    increments_fine =  np.sqrt(h_fine) * np.random.randn(N_fine)

    for i in range(1,i_max+1):
        N = 2**i
        h = T/N

        #Factor to reshape array for coarser grids
        factor = N_fine//N

        #Increments on coarser grid
        increments = increments_fine.reshape(N, factor).sum(axis=1)
        W = np.zeros(N+1)

        #Brownian path
        for j in range(1,N+1):
            W[j] = W[j-1] + increments[j-1]

        #Time axis
        t = np.linspace(0,T,N+1)
    
        plt.plot(t,W, label=f"h=2^-{i}")
        plt.title("Brownian motion sample path at different resolutions")
        plt.xlabel("t")
        plt.ylabel("W(t)")
        plt.legend()


#brownian_path()
#plt.show()


#Question 5 

def compute_x(i_max=10):
    mu = 2
    sigma = 1
    T = 1

    np.random.seed(321)

    #Increments on fine grid
    N_fine = 2**i_max
    h_fine = T/N_fine
    t_fine = np.linspace(0,T,N_fine+1)
    increments_fine =  np.sqrt(h_fine) * np.random.randn(N_fine)

    W_fine = np.zeros(N_fine+1)

    #Browinan path
    for n in range(1,N_fine+1):
        W_fine[n] = W_fine[n-1] + increments_fine[n-1]
    
    #Stochastic process
    X = np.exp((mu-(sigma**2)/2)*t_fine+sigma*W_fine)

    plt.plot(t_fine,X, label= "X(t)", color="black")

    for i in range(1,i_max+1):
        N = 2**i
        h = T/N

        #Factor and increments
        factor = N_fine//N
        increments = increments_fine.reshape(N, factor).sum(axis=1)

        #Brownian path
        W = np.zeros(N+1)
        for j in range(1,N+1):
            W[j] = W[j-1] + increments[j-1]

        Xh = np.zeros(N+1)  
        Xh[0] = 1

        #Estimate of Xh    
        for m in range(1,N+1):
            Xh[m] = (1+h*mu)*Xh[m-1] +sigma*Xh[m-1]*increments[m-1]
            
        #Time axis
        t = np.linspace(0,T,N+1)
        plt.plot(t,Xh, label=f"$X_{{h=2^{{-{i}}}}}$")
    
    plt.title("Simulation of Stochastic process")
    plt.xlabel("Time (t)")
    plt.ylabel("X(t)")
    plt.legend()

#compute_x()
#plt.show()       


#Question 6


def compute_strong_error(i_max=10):
    mu = 2
    sigma = 1
    T = 1
    M = 1000
    np.random.seed(321)
    
    h_values = []
    strong_errors = []



    for i in range(1,i_max+1):
        N = 2**i
        h = T/N
        h_values.append(h)


        sum_squared_error = 0

        for m in range(1,M+1):
            N_fine = 2**i_max
            h_fine = T/N_fine
            t_fine = np.linspace(0,T,N_fine+1)

            #Increments on fine grid
            increments_fine =  np.sqrt(h_fine) * np.random.randn(N_fine)

            W_fine = np.zeros(N_fine+1)

            #Brownian path
            for n in range(1,N_fine+1):
                W_fine[n] = W_fine[n-1] + increments_fine[n-1]
            
            #Stochastic process at T
            X_T = np.exp((mu-(sigma**2)/2)*T+sigma*W_fine[-1])



            factor = N_fine//N
            increments = increments_fine.reshape(N, factor).sum(axis=1)
            #Brownian path 
            W = np.zeros(N+1)
            for j in range(1,N+1):
                W[j] = W[j-1] + increments[j-1]

            Xh = np.zeros(N+1)  
            Xh[0] = 1

            #Estimate of stocastic process
            for n in range(1,N+1):
                Xh[n] = (1+h*mu)*Xh[n-1] +sigma*Xh[n-1]*increments[n-1]

            #Errors
            error = X_T- Xh[-1]
            sum_squared_error += error**2

        #Strong error
        strong_error = np.sqrt(sum_squared_error/M)
        strong_errors.append(strong_error)
        print(X_T)
        print(Xh)
    print(strong_errors)
    plt.loglog(h_values,strong_errors, 'o-', label= "Strong error")
    plt.loglog(h_values, np.sqrt(h_values), 'k--', label= "Reference slope")
    plt.xlabel("h")
    plt.ylabel("Strong errors")
    plt.title("Strong errors")
    plt.legend()

compute_strong_error()


plt.show()


#Question 7

def compute_weak_error(i_max=10, M=1000):
    mu = 2
    sigma = 1
    T = 1
    np.random.seed(321)

    h_values = []
    weak_errors = []

    # Fine grid for "exact" reference process X(T)
    N_fine = 2**i_max
    h_fine = T / N_fine

    for i in range(1, i_max + 1):
        N = 2**i
        h = T / N
        h_values.append(h)

        Xh_T_values = []

        for m in range(M):
            #Generate Brownian increments for fine path
            increments_fine = np.sqrt(h_fine) * np.random.randn(N_fine)
            factor = N_fine // N
            increments = increments_fine.reshape(N, factor).sum(axis=1)

            #simulate approximation Xh
            Xh = np.zeros(N + 1)
            Xh[0] = 1
            for n in range(1, N + 1):
                Xh[n] = (1 + h * mu) * Xh[n - 1] + sigma * Xh[n - 1] * increments[n - 1]

            Xh_T_values.append(Xh[-1])

        #Monte Carlo estimate of E[Xh(T)]
        E_Xh = np.mean(Xh_T_values)
        E_X_exact = np.exp(mu)  # since E[X(1)] = exp(mu)

        weak_error = abs(E_X_exact - E_Xh)
        weak_errors.append(weak_error)


    plt.loglog(h_values, weak_errors, 'o-', label="Weak error")
    plt.loglog(h_values, h_values, 'k--', label="Reference slope h")
    plt.xlabel("h")
    plt.ylabel("Weak error")
    plt.title("Weak error convergence")
    plt.legend()
    plt.show()

#compute_weak_error()


#Question 8


def compute_weak_error_phi(i_max=10, M=1000):
    mu = 2
    sigma = 1
    T = 1
    np.random.seed(321)

    h_values = []
    weak_errors = []

    #Exact expectation for phi(x) = x^2
    E_phi_exact = np.exp(2*mu + sigma**2)

    #Fine grid
    N_fine = 2**i_max
    h_fine = T / N_fine

    for i in range(1, i_max + 1):
        N = 2**i
        h = T / N
        h_values.append(h)

        sum_phi_Xh = 0  # sum over all M paths

        for m in range(M):
            #Generate fine increments
            increments_fine = np.sqrt(h_fine) * np.random.randn(N_fine)

            #increments to coarser grid
            factor = N_fine // N
            increments = increments_fine.reshape(N, factor).sum(axis=1)

            #simulate Xh path
            Xh = np.zeros(N + 1)
            Xh[0] = 1
            for n in range(1, N + 1):
                Xh[n] = (1 + h*mu)*Xh[n-1] + sigma*Xh[n-1]*increments[n-1]

            #phi(Xh(T)) = Xh(T)^2
            sum_phi_Xh += Xh[-1]**2

        #monte Carlo estimate
        E_phi_Xh = sum_phi_Xh / M

        #Weak error
        weak_error = abs(E_phi_exact - E_phi_Xh)
        weak_errors.append(weak_error)


    plt.loglog(h_values, weak_errors, 'o-', label=r"Weak error ($\varphi(x)=x^2$)")
    plt.loglog(h_values, h_values, 'k--', label="Reference slope h")
    plt.xlabel("h")
    plt.ylabel("Weak error")
    plt.title("Weak error for φ(x)=x²")
    plt.legend()
    plt.show()

#compute_weak_error_phi()