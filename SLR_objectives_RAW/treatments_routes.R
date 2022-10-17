
figs <- list[[]]
for(s in unique(df1$targetHost)){
    figs[[s]] <- plot_ly(orientation='h') %>%
        add_boxplot(data=df1, y = ~testSubstance, x = ~efficacy, color = ~route)%>%
        layout(title = title,
               yaxis = list(title = 'Substance or combination of substances tested'),
               xaxis = list(title = 'Efficacy %'),
               showlegend=T,boxmode = "group")

}


fig <- subplot(unlist(figs), nrows = length(figs)) %>%


    fig%>% layout(font=plots.font)


##################

fig <- plot_ly(orientation='h')#), line=list(color='gray'), height=400, width=600)


for(s in unique(df1$targetHost)){
    for (r in unique(df1$route)){
        if(dim(df1[df1$targetHost==s&df1$route==r,])[1]>1)
        fig <- fig %>% add_boxplot(data=df1[df1$targetHost==s&df1$route==r,],
                                   y = ~testSubstance, x = ~efficacy,
                                   color = ~targetHost
            , name=as.character(s))

    }
}

fig <- fig %>% layout(
    title = '',
    yaxis = list(
        autorange = TRUE,
        categoryorder = "category descending",
        domain = c(0, 1),
        range = c(-0.5, 3.5),
        showline = TRUE,
        title = "",
        type = "category"
    ),
    margin = list(
        r = 10,
        t = 25,
        b = 40,
        l = 110
    ),
    legend = list(
        x = 0.986145833333,
        y = 0.936263886049
    ),
    shapes = list(
        list(
            line = list(
                color = "rgba(68, 68, 68, 0.5)",
                width = 1
            ),
            type = "line",
            x0 = -0.3,
            x1 = 1.2,
            xref = "paper",
            y0 = 0.5,
            y1 = 0.5,
            yref = "paper"
        ),
        list(
            line = list(
                color = "rgba(68, 68, 68, 0.63)",
                width = 1
            ),
            type = "line",
            x0 = -0.3,
            x1 = 1.2,
            xref = "paper",
            y0 = 1,
            y1 = 1,
            yref = "paper"
        )
    ),
    annotations = list(
        list(
            x = -0.0951769406393,
            y = 1.06972670892,
            showarrow = FALSE,
            text = "Subgroup",
            xref = "paper",
            yref = "paper"
        ),
        list(
            x = -0.235516552511,
            y = 1.07060587474,
            showarrow = FALSE,
            text = "Group",
            xref = "paper",
            yref = "paper"
        ),
        list(
            x = -0.235516552511,
            y = 0.922906017856,
            showarrow = FALSE,
            text = as.character(unique(df1$route)[1]),
            xref = "paper",
            yref = "paper"
        ),
        list(
            x = -0.235516552511,
            y = 0.375,
            showarrow = FALSE,
            text = as.character(unique(df1$route)[2]),
            xref = "paper",
            yref = "paper"
        )
    )
)%>%layout(plot_bgcolor='#e5ecf6',
           xaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'),
           yaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff')
)

fig%>% layout(font=plots.font)












figs <- list()
annotations <- list()
sp=0
for(s in unique(df1$targetHost)){
    if(dim(df1[df1$targetHost==s,])[1]>1){
    figs[[s]] <- plot_ly(orientation='h') %>%
        add_boxplot(data=df1[df1$targetHost==s,], y = ~testSubstance,
                    x = ~efficacy, color = ~route)
                    #legendgroup=~route)

    annotations[[s]] <- list(
        x = 0.2,
        y = (1/length(unique(df1$targetHost)))*(length(unique(df1$targetHost))-sp),
        text = as.character(s),
        xref = "paper",
        yref = "paper",
        xanchor = "center",
        yanchor = "bottom",
        showarrow = FALSE
    )

    sp=sp+1
    }
}


fig <- subplot(figs, nrows = length(figs),shareX = TRUE)%>%
    layout(title = "",
           yaxis = list(title = 'Substance or combination of substances tested'),
           xaxis = list(title = 'Efficacy %'),
           #showlegend=T,
           boxmode="group",
           plot_bgcolor='#e5ecf6',
           annotations = annotations)


    fig%>% layout(font=plots.font)



    annotations = list()
    for
        list(
            x = 0.2,
            y = 1.0,
            text = "Plot 1",
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            yanchor = "bottom",
            showarrow = FALSE
        ),
        list(
            x = 0.8,
            y = 1,
            text = "Plot 2",
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            yanchor = "bottom",
            showarrow = FALSE
        ),
        list(
            x = 0.2,
            y = 0.45,
            text = "Plot 3",
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            yanchor = "bottom",
            showarrow = FALSE
        ),
        list(
            x = 0.8,
            y = 0.45,
            text = "Plot 4",
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            yanchor = "bottom",
            showarrow = FALSE
        ))

    fig <- fig %>%layout(annotations = annotations)




