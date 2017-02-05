%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SESSION1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear all;
clc;

% 1
nnd11gn;

% 2.2 and 2.3
x=linspace(0,1,21);
y=sin(0.7*pi*x);
plot(x,y,'-');
plot_xy = plot(x,y, '*');

% 2.4
net=fitnet(2); % Row vector of one or more hidden layer sizes, one hidden layer of size 2
net=configure(net,x,y);
net.inputs{1}.processFcns={};
net.outputs{2}.processFcns={};
[net, tr]=train(net,x,y);
yhat=net(x);

% 2.5 Get the first bias terms and the first set of weights
[biases, weights]=hidden_layer_weights(net);
% 2.6 Get the activation function
act_function=hidden_layer_transfer_function(net);
% bias was obtained but the activations are missing, ask in lass next time.

% 2.6
biases = net.b{1};
weights = net.IW{1,1}
 
x_p_1 = tansig(biases(1) + weights(1)*x);
x_p_2 = tansig(biases(2) + weights(2)*x);
plot(1:21, x_p_1)
xlabel('p')
ylabel('x^1_p')
hold on;
plot(1:21,x_p_2,'-r')
plot(1:21,y,'-g')
%y_fit = tansig(net.b{2} + net.LW{2,1}(1)*x_p_1 + net.LW{2,1}(2)*x_p_2);
output_p = tansig(net.b{2} + net.LW{2,1}(1)*x_p_1 + net.LW{2,1}(2)*x_p_2)
plot(1:21,output_p,'-o')
legend('x^1_p','x^2_p','y','output_p')
hold off;


%% Alternative:  Plot the values of y, a_1^2 and a_2^2 against p
% a_1^2
a1=tansig(biases(1)+weights(1)*x);
a2=tansig(biases(2)+weights(2)*x);
%%
plot(a1)
hold on;
plot(a2)
hold on;
plot(y)
legend('a1','a2','y')
%% Obtain estimate parameters for the output layer

[biases2, weights2]=output_layer_weights(net);
act_function_output=output_layer_transfer_function(net);
%% Here the output is a linear combination of the neurons in the hidden layer, no other function is applied 

a3=biases2+weights2(1)*a1+weights2(2)*a2;

%% Comparing the results

plot(x,y,'r-');
hold on;
%plot(x, yhat,'b');
%hold on;
plot(x,a3,'g*');
legend('y','a3')

% 3.1
train_x = linspace(-1, 1, 100);
train_y = sin(2*pi*train_x) + 0.5*randn(size(train_x));
val_x = linspace(-0.9, 0.9, 100);
val_y = sin(2*pi*val_x) + 0.5*randn(size(val_x));
x = [train_x val_x]; 
y = [train_y val_y]; 

net = fitnet(5, 'trainscg');
net.divideFcn = 'divideind';
net.divideParam = struct('trainInd', 1:100,'valInd', 101:200,'testInd', []);
[net, tr] = train(net, x, y);

train_yhat = net(train_x);
plot(train_x, train_y, 'r*');
hold on;
plot(train_x, trainyhat, '-');
plot(train_x, sin(2*pi*train_x), 'g-');
hold off;
legend('Trainingset', 'Approximated_function', 'True_Function');






