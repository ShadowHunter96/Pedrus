package cz.bbn.cerberus.task.ui;

import com.vaadin.flow.component.ClickEvent;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.ComponentEventListener;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.OptionalParameter;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.appcard.AppCardGridComponent;
import cz.bbn.cerberus.commons.component.ui.slidetab.NewEntityButtonsComponent;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.entitynew.EntityNewType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.note.NoteTypeEnum;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.task.TaskComponentOperation;
import cz.bbn.cerberus.task.ui.component.TaskFilterComponent;
import cz.bbn.cerberus.task.ui.component.TaskGridComponent;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.List;


@Route(value = TaskView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.TASK_VIEW)
@Slf4j
public class TaskView extends AppView implements HasUrlParameter<String> {

    public static final String ROUTE = "task-list";

    private final AppEnv appEnv;
    private final TaskComponentOperation taskComponentOperation;
    private final EntityNewComponentOperation entityNewComponentOperation;
    private final ListService listService;

    public TaskView(AppEnv appEnv, TaskComponentOperation taskComponentOperation,
                    EntityNewComponentOperation entityNewComponentOperation, ListService listService) {
        this.appEnv = appEnv;
        this.taskComponentOperation = taskComponentOperation;
        this.entityNewComponentOperation = entityNewComponentOperation;
        this.listService = listService;
    }

    private void initView(String params) {
        removeAll();
        setSizeFull();

        Button search = VaadinComponents.getSearchButton();
        TaskFilterComponent taskFilterComponent = new TaskFilterComponent(search, params,
                getHistoryBreadcrumbs(), listService.getUserDtoList(), true);
        TaskGridComponent grid = new TaskGridComponent(
                appEnv, taskFilterComponent,
                taskComponentOperation);

        List<String> newTitleList = new ArrayList<>();
        newTitleList.add(Transl.get("Add task"));
        newTitleList.add(Transl.get("Task from template"));

        List<ComponentEventListener<ClickEvent<? extends Component>>> clickEventList = new ArrayList<>();
        clickEventList.add(taskComponentOperation.getAddNewClickEvent(grid, false));
        clickEventList.add(taskComponentOperation.getAddNewClickEvent(grid, true));

        AppCardGridComponent card = new AppCardGridComponent(Transl.get("Task list"), Permission.TASK_EDIT,
                newTitleList, clickEventList, entityNewComponentOperation, NoteTypeEnum.ANY, null);
        card.addNewEntitySlideTab(new NewEntityButtonsComponent(entityNewComponentOperation, EntityNewType.SALES));
        card.setId(RobotFrameworkVariables.TASK_VIEW_CARD_ID.getValue());

        card.add(taskFilterComponent);
        card.add(grid);

        add(card);
        grid.loadData();
        search.addClickListener(buttonClickEvent -> {
            grid.loadData();
            taskFilterComponent.fillUrl();
        });
    }

    @Override
    public void setParameter(BeforeEvent beforeEvent, @OptionalParameter String params) {
        initView(params);
    }

}
