function plot_cm(P)
    // Numero di stati
    num_states = size(P, 1);

    // Coordinate dei nodi su un cerchio
    theta = linspace(0, 2*%pi, num_states + 1);
    x = cos(theta(1:num_states));
    y = sin(theta(1:num_states));

    // Prepara la finestra grafica
    clf;
    scf(0);
    plot2d([], [], -1, "111", "", [-1.2, -1.2, 1.2, 1.2]); // Imposta assi per evitare tagli
    h = gca();
    h.isoview = "on"; // Assicura che il cerchio sia un cerchio
    h.axes_visible = ["off", "off"]; // Nasconde gli assi

    // Disegna i nodi
    for i = 1:num_states
        xstring(x(i), y(i), string(i));
        // Disegna un cerchio bianco sotto per migliorare la leggibilità delle linee
        xcircle(x(i), y(i), 0.05, "fill", "white");
        xcircle(x(i), y(i), 0.05);
    end

    // Disegna le transizioni
    for i = 1:num_states
        for j = 1:num_states
            if P(i,j) > 0 then
                
                // === GESTIONE DEI CAPPI (i -> i) ===
                if i == j then
                    // Disegna un cappio (arco) vicino al nodo
                    center_angle = atan(y(i), x(i));
                    radius = 0.15;
                    loop_center_x = x(i) + radius * cos(center_angle);
                    loop_center_y = y(i) + radius * sin(center_angle);
                    // Disegna un cerchio e l'etichetta
                    xarc(loop_center_x-0.08, loop_center_y+0.08, 0.16, 0.16, 0, 360*64);
                    // Posiziona l'etichetta ancora più esternamente
                    xstring(x(i) + 2.5*radius*cos(center_angle), y(i) + 2.5*radius*sin(center_angle), string(P(i,j)));
                
                // === GESTIONE DELLE TRANSIZIONI NORMALI (i -> j) ===
                else
                    // Vettore dalla partenza alla fine
                    dx = x(j) - x(i);
                    dy = y(j) - y(i);
                    len = sqrt(dx^2 + dy^2);
                    
                    // Normalizza per ottenere il versore di direzione
                    ux = dx / len;
                    uy = dy / len;
                    
                    // Riduci la linea per non sovrapporla ai cerchi dei nodi
                    offset = 0.05; // Raggio del cerchio
                    x_start = x(i) + offset * ux;
                    y_start = y(i) + offset * uy;
                    x_end = x(j) - offset * ux;
                    y_end = y(j) - offset * uy;
                    
                    xarrows([x_start], [y_start], [x_end-x_start], [y_end-y_start], 0.03, 1, "filled");

                    // === CORREZIONE POSIZIONE ETICHETTE PER EVITARE SOVRAPPOSIZIONI ===
                    // Calcola un piccolo offset perpendicolare alla linea
                    offX = -uy * 0.05;
                    offY = ux * 0.05;
                    xm = (x_start + x_end)/2 + offX;
                    ym = (y_start + y_end)/2 + offY;
                    xstring(xm, ym, string(P(i,j)));
                end
            end
        end
    end
    xtitle("Grafo della Catena di Markov");
endfunction
