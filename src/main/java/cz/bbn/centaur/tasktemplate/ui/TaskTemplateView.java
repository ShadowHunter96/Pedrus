package cz.bbn.cerberus.tasktemplate.ui;

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
import cz.bbn.cerberus.tasktemplate.TaskTemplateComponentOperation;
import cz.bbn.cerberus.tasktemplate.ui.component.TaskTemplateFilterComponent;
import cz.bbn.cerberus.tasktemplate.ui.component.TaskTemplateGridComponent;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;

@Route(value = TaskTemplateView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.TASK_TEMPLATE_VIEW)
@Slf4j
public class TaskTemplateView extends AppView implements HasUrlParameter<String> {

    public static final String ROUTE = "task-template-list-view";

    private final TaskTemplateComponentOperation componentOperation;
    private final EntityNewComponentOperation entityNewComponentOperation;
    private final AppEnv appEnv;

    public TaskTemplateView(TaskTemplateComponentOperation componentOperation,
                            EntityNewComponentOperation entityNewComponentOperation, AppEnv appEnv) {
        this.componentOperation = componentOperation;
        this.entityNewComponentOperation = entityNewComponentOperation;
        this.appEnv = appEnv;
    }

    private void initView(String params) {
        removeAll();
        setSizeFull();

        Button search = VaadinComponents.getSearchButton();
        TaskTemplateFilterComponent taskTemplateFilterComponent = new TaskTemplateFilterComponent(search, params,
                getHistoryBreadcrumbs(), componentOperation.getAllowedUsers(), componentOperation.getAllowedRoles());
        TaskTemplateGridComponent grid = new TaskTemplateGridComponent(
                appEnv, taskTemplateFilterComponent,
                componentOperation);

        AppCardGridComponent card = new AppCardGridComponent(Transl.get("Task template list"),
                Permission.TASK_TEMPLATE_EDIT, Transl.get("Add task template"),
                componentOperation.getAddNewClickEvent(grid),
                entityNewComponentOperation, NoteTypeEnum.ANY, null);
        card.addNewEntitySlideTab(new NewEntityButtonsComponent(entityNewComponentOperation, EntityNewType.SALES));
        card.setId(RobotFrameworkVariables.TASK_VIEW_CARD_ID.getValue());

        card.add(taskTemplateFilterComponent);
        card.add(grid);

        add(card);
        grid.loadData();
        search.addClickListener(buttonClickEvent -> {
            grid.loadData();
            taskTemplateFilterComponent.fillUrl();
        });
    }

    @Override
    public void setParameter(BeforeEvent beforeEvent, @OptionalParameter String params) {
        initView(params);
    }
}
