function [grad1, grad2, grad3] = gradients(x,y,t)
    % Assuming n touchpoints on the screen, the inputs are:
        % x: touchpoint's x coords as a n sized vector
        % y: touchpoint's y coords as a n sized vector
        % t: time of touchpoints as a n sized vector
        
        
    % first gradient in x direction at each timepoint t as a vector
    gradx1 = gradient(x,t);
    % first gradient in y direction at each timepoint t as a vector
    grady1 = gradient(y,t);
    
    % second gradient vector in x direction at each timepoint t
    gradx2 = gradient(gradx1,t);
    % second gradient vector in y direction at each timepoint t
    grady2 = gradient(grady1,t);
    
    % third gradient vector in x direction at each timepoint t
    gradx3 = gradient(gradx2,t);
    % third gradient vector in y direction at each timepoint t
    grady3 = gradient(grady2,t);
    
    % apply Pythagoras' theorem to add the orthogonal x and y components
    % the outputs are:
    % grad1 is the velocity at each time t 
    % grad2 is the acceleration at each time t
    % grad3 is the jerk at each time t
    grad1 = sqrt(gradx1.^2 + grady1.^2)
    grad2 = sqrt(gradx2.^2 + grady2.^2)
    grad3 = sqrt(gradx3.^2 + grady3.^2)
