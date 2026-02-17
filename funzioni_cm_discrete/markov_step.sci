function next = markov_step(P, current_state)
    r = rand();
    cum = cumsum(P(current_state, :));
    next = find(cum >= r, 1);
endfunction
