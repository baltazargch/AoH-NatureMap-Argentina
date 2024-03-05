sidebarMenu(id = 'tabs',
            menuItem('Inicio', tabName = 'tabInicio', icon = icon("cog")),
            menuItem('Conservación', tabName = 'tabCon', icon= icon('paw')),
            menuItem('Hábitats', tabName = 'tabHabs', icon = icon('tree')),
            menuItem('Ecorregiones', tabName = 'tabEco', icon = icon('pagelines')),
            menuItem('Elevación', tabName = 'tabElev', icon= icon('map-pin')),
            menuItem('Visor y restaurador', tabName = 'tabVisor', icon=icon('cog')),
            menuItem('Área de Hábitat', tabName = 'tabAOH', icon=icon('globe-americas')),
            menuItem('Resumen avance', tabName = 'tabResumen', icon=icon('table')), 
            menuItemOutput('tabInvitados'), 
            column(11,align = "center",offset = 0,
                   br(),br(), div(style="height: 150px;",
                     actionButton("stop", "Finalizar y \ngenerar informe",
                                style="height: 150px;
                                background-color: #CD5969;
                                color: white;
                                white-space:normal;
                                font-size: 18px;
                                border-radius: 8px;
                                box-shadow: 0 8px 16px 0 rgba(0,0,0,0.2), 0 6px 20px 0 rgba(0,0,0,0.19);",
                                width = '100%')
                     )
                   
                   )
)