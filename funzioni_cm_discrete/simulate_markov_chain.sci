
exec('C:\Users\feran\Documents\Magistrale\PSAST\funzioni_cm_discrete\markov_step.sci');


function [counts, path] = simulate_markov_chain(P, starting_state, transition_num)
    
    num_states = size(P, 1);
    
    if(starting_state < 1 | starting_state > num_states)
       current_state = floor( 1 + rand() * num_states);
    else
        current_state = starting_state;
    end
    
    counts = zeros(num_states, 1);
    path = zeros(transition_num, 1);
    
    for transition = 1:transition_num
        path(transition) = current_state;
        counts(current_state) = counts(current_state) + 1;
        current_state = markov_step(P, current_state);
    end
    
    
endfunction
