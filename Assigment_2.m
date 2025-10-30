% load data
data = load('diabetes.txt');
%histogram(data)

%Question i
%{
%Gaussian mixture model, with fixed switching problem
%Estimates of mu and Sigma from looking at the histogram
startpar.mu = [80; 130; 190];
startpar.Sigma(:,:,1) = 40;
startpar.Sigma(:,:,2) = 20;
startpar.Sigma(:,:,3) = 20;
startpar.ComponentProportion = [0.33,0.33,0.33];

GM = fitgmdist(data, 3, 'Start', startpar);

%Extract fitted means
GM.mu 
%Extract fitted fitted variance-covariance matrices
GM.Sigma

%Standard devation
sqrt(GM.Sigma)

%Extract mixing proprotions
GM.ComponentProportion

%}
%Question ii
%{
rng(123);

%Weights in GM model
weights = GM.ComponentProportion;
cum_weights = cumsum(weights);

%Simulate new data
for i = 1:500
    r = rand(1);
    k_star = find(r <= cum_weights,1);
    values(i) = normrnd(GM.mu(k_star), sqrt(GM.Sigma(k_star))); 
end


%Plot the histograms of original data and simulated data

figure;

subplot(1,2,1); 
histogram(data);
title('Original data');

subplot(1,2,2); 
histogram(values);
title('Simulated data');
%}


% Question iii
%{
rng(123);
%Number of estimates
B=2000;

for i = 1:B
    %Start values and sampled data

    sample_data = datasample(data, length(data));
    startpar.mu = [80; 130; 190];
    startpar.Sigma(:,:,1) = 40;
    startpar.Sigma(:,:,2) = 20;
    startpar.Sigma(:,:,3) = 20;
    startpar.ComponentProportion = [0.33,0.33,0.33];
    
    %Model
    GM = fitgmdist(sample_data, 3, 'Start', startpar);
    mu_values = GM.mu;

    %Mu values
    mu1_values(i) = mu_values(1);
    mu2_values(i) = mu_values(2);
    mu3_values(i) = mu_values(3);
 
    %Sigma values
    Sigma_values = sqrt(GM.Sigma); 

    Sigma1_values(i) = Sigma_values(1);
    Sigma2_values(i) = Sigma_values(2);
    Sigma3_values(i) = Sigma_values(3);
    
    %Component distribution values
    component_distribution = GM.ComponentProportion;

    component_distribution_1(i) = component_distribution(1);
    component_distribution_2(i) = component_distribution(2);
    component_distribution_3(i) = component_distribution(3);
    
    
end

figure;

%Plot the histograms

subplot(3,3,1)
histogram(mu1_values);
title(['Bootstrap of \mu_1', 1]);
xlabel(['\mu_1', 1]);
ylabel('Frequency');

subplot(3,3,2)
histogram(mu2_values);
title(['Bootstrap of \mu_2', 2]);
xlabel(['\mu_2', 2]);
ylabel('Frequency');


subplot(3,3,3)
histogram(mu3_values);
title(['Bootstrap of \mu_3', 3]);
xlabel(['\mu_3', 3]);
ylabel('Frequency');

%Calculate the confidence interval
for k = 1:3
    CI = prctile(eval(['mu', num2str(k), '_values']), [2.5 97.5]);
    disp(['95% CI for mu', num2str(k), ': ', num2str(CI)]);
end

subplot(3,3,4)
histogram(Sigma1_values);
title(['Bootstrap of \sigma_1', 1]);
xlabel(['\sigma_1', 1]);
ylabel('Frequency');

subplot(3,3,5)
histogram(Sigma2_values);
title(['Bootstrap of \sigma_2', 2]);
xlabel(['\sigma_2', 2]);
ylabel('Frequency');


subplot(3,3,6)
histogram(Sigma3_values);
title(['Bootstrap of \sigma_3', 3]);
xlabel(['\sigma_3', 3]);
ylabel('Frequency');

%Calculate the confidence interval
for k = 1:3
    CI = prctile(eval(['Sigma', num2str(k), '_values']), [2.5 97.5]);
    disp(['95% CI for sigma', num2str(k), ': ', num2str(CI)]);
end
subplot(3,3,7)
histogram(component_distribution_1);
title(['Bootstrap of \pi_1', 1]);
xlabel(['\pi_1', 1]);
ylabel('Frequency');

subplot(3,3,8)
histogram(component_distribution_2);
title(['Bootstrap of \pi_2', 2]);
xlabel(['\pi_2', 2]);
ylabel('Frequency');


subplot(3,3,9)
histogram(component_distribution_3);
title(['Bootstrap of \pi_3', 3]);
xlabel(['\pi_3', 3]);
ylabel('Frequency');

%Calculate confidence interval
for k = 1:3
    CI = prctile(eval(['component_distribution_', num2str(k)]), [2.5 97.5]);
    disp(['95% CI for pi', num2str(k), ': ', num2str(CI)]);
end

%}

%Question iv


rng(123);

%model to extract parameters from earlier question. Previous code is
%commented so this again to get parameters for generating new data


startpar.mu = [80; 130; 190];
startpar.Sigma(:,:,1) = 40;
startpar.Sigma(:,:,2) = 20;
startpar.Sigma(:,:,3) = 20;
startpar.ComponentProportion = [0.33,0.33,0.33];

GM_2 = fitgmdist(data, 3, 'Start', startpar);
 



%Number of estimates
B=2000;

sigma = sqrt(GM_2.Sigma);
mu_value = GM_2.mu;

%Start values
startpar.mu = GM_2.mu;

startpar.Sigma = zeros(1,1,3);
startpar.Sigma(:,:,1) = GM_2.Sigma(1);   
startpar.Sigma(:,:,2) = GM_2.Sigma(2);
startpar.Sigma(:,:,3) = GM_2.Sigma(3);


startpar.ComponentProportion = [0.5520,0.2980,0.1500];
 
n = length(data);
weights = startpar.ComponentProportion;
cum_weights = cumsum(weights);

for i = 1:B
    %Initialize empty vector for values
    x_star = zeros(n,1);

    %Generate data
    for j = 1:n
        r = rand(1);
        k_star = find(r <= cum_weights, 1);
        x_star(j) = normrnd(mu_value(k_star), sigma(:,:,k_star)); 
    end    

    %Model
    GM = fitgmdist(x_star, 3, 'Start', startpar);

    %Mu values
    mu_values = GM.mu;
    mu1_values(i) = mu_values(1);
    mu2_values(i) = mu_values(2);
    mu3_values(i) = mu_values(3);

    %Sigma values    
    Sigma_values = sqrt(GM.Sigma); 
    Sigma1_values(i) = Sigma_values(1);
    Sigma2_values(i) = Sigma_values(2);
    Sigma3_values(i) = Sigma_values(3);
    
    %Component distribution values    
    component_distribution = GM.ComponentProportion;
    component_distribution_1(i) = component_distribution(1);
    component_distribution_2(i) = component_distribution(2);
    component_distribution_3(i) = component_distribution(3);
end


%Plot the histograms
figure;

subplot(3,3,1)
histogram(mu1_values);
title(['Bootstrap of \mu_1', 1]);
xlabel(['\mu_1', 1]);
ylabel('Frequency');

subplot(3,3,2)
histogram(mu2_values);
title(['Bootstrap of \mu_2', 2]);
xlabel(['\mu_2', 2]);
ylabel('Frequency');


subplot(3,3,3)
histogram(mu3_values);
title(['Bootstrap of \mu_3', 3]);
xlabel(['\mu_3', 3]);
ylabel('Frequency');


subplot(3,3,4)
histogram(Sigma1_values);
title(['Bootstrap of \sigma_1', 1]);
xlabel(['\sigma_1', 1]);
ylabel('Frequency');

subplot(3,3,5)
histogram(Sigma2_values);
title(['Bootstrap of \sigma_2', 2]);
xlabel(['\sigma_2', 2]);
ylabel('Frequency');


subplot(3,3,6)
histogram(Sigma3_values);
title(['Bootstrap of \sigma_3', 3]);
xlabel(['\sigma_3', 3]);
ylabel('Frequency');


subplot(3,3,7)
histogram(component_distribution_1);
title(['Bootstrap of \pi_1', 1]);
xlabel(['\pi_1', 1]);
ylabel('Frequency');

subplot(3,3,8)
histogram(component_distribution_2);
title(['Bootstrap of \pi_2', 2]);
xlabel(['\pi_2', 2]);
ylabel('Frequency');


subplot(3,3,9)
histogram(component_distribution_3);
title(['Bootstrap of \pi_3', 3]);
xlabel(['\pi_3', 3]);
ylabel('Frequency');

%}

% Question v
%{
bigger_than_85_mu1 = find(mu1_values >85);

prob = length(bigger_than_85_mu1)/length(mu1_values);

bigger_than_85_mu2 = find(mu2_values > 85);

%}