package cz.bbn.cerberus.marekdemo.ui;

import com.vaadin.flow.component.grid.Grid;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.router.Route;
import com.vaadin.flow.spring.annotation.SpringComponent;
import cz.bbn.cerberus.commons.component.ui.appcard.AppCard;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.marekdemo.MarekService;
import cz.bbn.cerberus.marekdemo.dto.MarekDTO;
import cz.bbn.cerberus.marekdemo.ui.component.MarekForm;
import org.springframework.context.annotation.Scope;

import javax.annotation.security.PermitAll;
import java.util.List;

/**
 * Created by marek.vu on 05.10.2023.
 */
@Route(value = MarekView.ROUTE, layout = MainLayout.class)
@PermitAll
@Scope(value = "vaadin-ui")
@SpringComponent
public class MarekView extends VerticalLayout {
    Grid<MarekDTO> grid = new Grid<>(MarekDTO.class);
    MarekService marekService;
    MarekForm marekForm;
    //TextField filterText = new TextField();

    private final AppEnv appEnv;


    public static final String ROUTE = "marek";


    private final EntityNewComponentOperation entityNewComponentOperation;

    public MarekView(MarekService marekService, AppEnv appEnv, EntityNewComponentOperation entityNewComponentOperation) {
        this.marekService = marekService;
        this.appEnv = appEnv;
        this.entityNewComponentOperation = entityNewComponentOperation;
        setSizeFull();
        configureGrid();
        //configureForm();

        List<MarekDTO> taskDtos = marekService.getAllTaskDtos();
        grid.setItems(taskDtos);
        AppCard appCard = new AppCard(entityNewComponentOperation);
        marekForm = new MarekForm(marekService.getSaveAction(grid));
        VerticalLayout verticalLayout = new VerticalLayout();
        grid.setSizeFull();
        grid.setMinHeight("30em");
        verticalLayout.add(marekForm, grid);
        appCard.add(verticalLayout);
        add(appCard);

    }

    private void configureGrid() {
        grid.addClassName("TaskEntity1-grid");
        grid.setSizeFull();
        grid.setColumns("id", "name", "description");
        grid.getColumns().forEach(col -> col.setAutoWidth(true));

        // Add item click listener here
        grid.addItemClickListener(event -> {
            MarekDTO selectedDto = event.getItem();
            //populateForm(selectedDto);
        });
    }

    /*

    // click listender
    private void populateForm(MarekDTO marekDTO) {
        marekForm.binder.setBean(marekDTO);
    }


    private void configureForm(){
        marekForm = new MarekForm(Collections.emptyList(),marekService.getSaveAction());

        marekForm.setWidth("25em");
        marekForm.setSaveListener(this::saveMarek);

//        marekForm.addDeleteListener(this::deleteMarek);

    }

    private void saveMarek(MarekForm event){
        MarekDTO marekDTO = event.getMarekDTO();
        MarekEntity marekEntity = new MarekEntity();
        MarekFactory.fillEntity(marekEntity,marekDTO);
        marekService.saveMarek(marekEntity);
        updateList();
    }

    private Component getContent(){
        HorizontalLayout content = new HorizontalLayout(grid,marekForm);
        content.setFlexGrow(2, grid);
        content.setFlexGrow(1, marekForm);
        content.addClassName("content");
        content.setSizeFull();
        return content;
    }

    //toolbar
    private HorizontalLayout getToolbar(){
        filterText.setPlaceholder("Filter by name...");//cerberus_new_db
        filterText.setClearButtonVisible(true);
        filterText.setValueChangeMode(ValueChangeMode.EAGER);
        filterText.addValueChangeListener(e->updateList());

        Button addMarekButton = new Button("Add Marek");
        var toolBar = new HorizontalLayout(filterText, addMarekButton);
        return toolBar;

    }
    private void updateList(){
        grid.setItems(marekService.findAllMarekDtos(filterText.getValue()));
    }


//    private void deleteMarek(MarekForm.DeleteEvent event) {
//        MarekDTO dto = event.getMarekDTO();
//        marekService.deleteMarekbyId(event.getMarekDTO().getId());
//        updateList();
//    }

     */

    //
}
    
