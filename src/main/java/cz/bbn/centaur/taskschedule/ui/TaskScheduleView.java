package cz.bbn.cerberus.taskschedule.ui;

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
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.note.NoteTypeEnum;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.taskschedule.TaskScheduleComponentOperation;
import cz.bbn.cerberus.taskschedule.ui.component.TaskScheduleFilterComponent;
import cz.bbn.cerberus.taskschedule.ui.component.TaskScheduleGridComponent;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;

@Route(value = TaskScheduleView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.TASK_SCHEDULE_VIEW)
@Slf4j
public class TaskScheduleView extends AppView implements HasUrlParameter<String> {

    public static final String ROUTE = "task-schedule-list-view";

    private final TaskScheduleComponentOperation componentOperation;
    private final EntityNewComponentOperation entityNewComponentOperation;
    private final AppEnv appEnv;

    public TaskScheduleView(TaskScheduleComponentOperation componentOperation,
                            EntityNewComponentOperation entityNewComponentOperation, AppEnv appEnv) {
        this.componentOperation = componentOperation;
        this.entityNewComponentOperation = entityNewComponentOperation;
        this.appEnv = appEnv;
    }

    private void initView(String params) {
        removeAll();
        setSizeFull();

        Button search = VaadinComponents.getSearchButton();
        TaskScheduleFilterComponent taskScheduleFilterComponent = new TaskScheduleFilterComponent(search, params,
                getHistoryBreadcrumbs(), componentOperation.getAllowedUsers(), componentOperation.getAllTaskTypeList());
        TaskScheduleGridComponent grid = new TaskScheduleGridComponent(
                appEnv, taskScheduleFilterComponent, componentOperation);

        AppCardGridComponent card = new AppCardGridComponent(Transl.get("Task schedule list"),
                Permission.TASK_SCHEDULE_EDIT, Transl.get("Add task schedule"),
                componentOperation.getAddNewClickEvent(grid),
                entityNewComponentOperation, NoteTypeEnum.ANY, null);
        card.addNewEntitySlideTab(new NewEntityButtonsComponent(entityNewComponentOperation, EntityNewType.SALES));
        card.setId(RobotFrameworkVariables.TASK_VIEW_CARD_ID.getValue());

        card.add(taskScheduleFilterComponent);
        card.add(grid);

        add(card);
        grid.loadData();
        search.addClickListener(buttonClickEvent -> {
            grid.loadData();
            taskScheduleFilterComponent.fillUrl();
        });
    }

    @Override
    public void setParameter(BeforeEvent beforeEvent, @OptionalParameter String params) {
        initView(params);
    }
}
