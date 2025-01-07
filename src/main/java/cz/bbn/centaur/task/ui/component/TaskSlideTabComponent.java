package cz.bbn.cerberus.task.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import cz.bbn.cerberus.commons.AppSlideTab;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.slidetab.CountIntIndicator;
import cz.bbn.cerberus.commons.component.ui.slidetab.SlideBarCountUpdateAction;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.task.TaskComponentOperation;
import cz.bbn.cerberus.task.dto.TaskFilterDto;
import cz.bbn.cerberus.translation.Transl;


public class TaskSlideTabComponent extends VerticalLayout {

    private final TaskComponentOperation taskComponentOperation;
    private final AppEnv appEnv;
    private final ObjectType objectType;
    private final String objectId;
    private final String subjectId;
    private final SubjectDto subjectDto;
    private final ListService listService;
    private TaskGridComponent grid;
    private CountIntIndicator taskIndicator;
    private TaskFilterDto countFilterDto;

    public TaskSlideTabComponent(TaskComponentOperation taskComponentOperation, AppEnv appEnv,
                                 ObjectType objectType, String objectId, SubjectDto subjectDto,
                                 ListService listService) {
        this.taskComponentOperation = taskComponentOperation;
        this.appEnv = appEnv;
        this.objectType = objectType;
        this.objectId = objectId;
        this.subjectId = subjectDto != null ? subjectDto.getId() : null;
        this.subjectDto = subjectDto;
        this.listService = listService;
        initComponent();
    }

    void initComponent() {
        removeAll();
        setSizeFull();

        HorizontalLayout filterLayout = new HorizontalLayout();
        filterLayout.setWidthFull();

        VerticalLayout buttonLayout = new VerticalLayout();
        buttonLayout.setMargin(false);
        buttonLayout.setPadding(false);
        buttonLayout.setWidth("10em");

        Button search = VaadinComponents.getSearchButton();
        TaskFilterComponent taskFilterComponent = new TaskFilterComponent(
                search, objectType, objectId, subjectId, listService.getUserDtoList(), true);
        taskFilterComponent.setWidthFull();

        countFilterDto = new TaskFilterDto();
        countFilterDto.setObjectId(objectId);
        countFilterDto.setObjectType(objectType);
        countFilterDto.setSubjectId(subjectId);


        if (SecurityUtils.hasPermission(Permission.TASK_EDIT)) {
            taskIndicator = new CountIntIndicator(taskComponentOperation.getTaskCount(countFilterDto));
            Button button = VaadinComponents.getNewButton(Transl.get("New task"));
            button.setWidth("10em");
            button.getElement().getStyle().set("margin-top", "2.3em");
            button.addClickListener(e -> taskComponentOperation.getAddNewClickEvent(grid,
                    subjectDto, objectType, objectId, this, false).onComponentEvent(e));
            buttonLayout.add(button);

            Button templateButton = VaadinComponents.getNewButton(Transl.get("Task from template"));
            templateButton.setWidth("10em");
            templateButton.addClickListener(e -> taskComponentOperation.getAddNewClickEvent(grid,
                    subjectDto, objectType, objectId, this, true).onComponentEvent(e));
            buttonLayout.add(templateButton);
        }

        grid = new TaskGridComponent(
                appEnv, taskFilterComponent, taskComponentOperation);

        grid.loadData();
        search.addClickListener(buttonClickEvent -> grid.loadData());

        filterLayout.add(buttonLayout);
        filterLayout.add(taskFilterComponent);
        this.add(filterLayout, grid);
    }

    public void setAppSlideTab(AppSlideTab appSlideTab) {
    }

    public TaskGridComponent getGrid() {
        return grid;
    }

    public SlideBarCountUpdateAction getCountUpdateAction() {
        return () -> {
            if (taskIndicator != null) {
                taskIndicator.setCount(taskComponentOperation.getTaskCount(countFilterDto));
            }
        };
    }

    public CountIntIndicator getTaskIndicator() {
        return taskIndicator;
    }
}
