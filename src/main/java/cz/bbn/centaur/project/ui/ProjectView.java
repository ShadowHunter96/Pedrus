package cz.bbn.cerberus.project.ui;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.OptionalParameter;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.appcard.AppCardGridComponent;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.slidetab.NewEntityButtonsComponent;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.entitynew.EntityNewType;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.note.NoteTypeEnum;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.project.ProjectService;
import cz.bbn.cerberus.project.dto.ProjectFilterDto;
import cz.bbn.cerberus.project.dto.ProjectSimpleDto;
import cz.bbn.cerberus.project.ui.component.ProjectFormFilterComponent;
import cz.bbn.cerberus.project.ui.component.ProjectGrid;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.UserService;
import lombok.extern.slf4j.Slf4j;

@Route(value = ProjectView.ROUTE, layout = MainLayout.class)
@Authorize({Permission.PROJECT_VIEW, Permission.PROJECT_LIST_VIEW})
@Slf4j
public class ProjectView extends AppView implements HasUrlParameter<String> {

    public static final String ROUTE = "app-project-list";

    private final AppEnv appEnv;
    private final ProjectService projectService;
    private final UserService userService;
    private final ListService listService;
    private final EntityNewComponentOperation entityNewComponentOperation;

    public ProjectView(AppEnv appEnv, ProjectService projectService,
                       UserService userService, ListService listService,
                       EntityNewComponentOperation entityNewComponentOperation) {
        this.appEnv = appEnv;
        this.projectService = projectService;
        this.userService = userService;
        this.listService = listService;
        this.entityNewComponentOperation = entityNewComponentOperation;
        setSizeFull();
    }

    private void initView(String params) {
        removeAll();

        Button search = VaadinComponents.getSearchButton();
        ProjectFormFilterComponent projectFilterComponent =
                new ProjectFormFilterComponent(search, userService.findUserList(), params, getHistoryBreadcrumbs());

        ProjectGrid grid = new ProjectGrid(
                getDeleteAction(), appEnv, getItemsAction(projectFilterComponent), listService.getSubjectMap());

        AppCardGridComponent card = new AppCardGridComponent(Transl.get("Project list"),
                Permission.PROJECT_EDIT, Transl.get("Add project"),
                entityNewComponentOperation.getNewProjectEvent(grid, null, null),
                entityNewComponentOperation, NoteTypeEnum.ANY, ObjectType.ANY
        );

        card.setId(RobotFrameworkVariables.PROJECT_VIEW_CARD_ID.getValue());
        card.add(projectFilterComponent);
        card.add(grid);

        card.addNewEntitySlideTab(new NewEntityButtonsComponent(entityNewComponentOperation, EntityNewType.SALES));
        add(card);
        grid.loadData();
        search.addClickListener(buttonClickEvent -> {
            grid.loadData();
            projectFilterComponent.fillUrl();
        });
    }

    private ItemsAction<ProjectSimpleDto> getItemsAction(ProjectFormFilterComponent projectFilterComponent) {
        return (query, orderList) -> {
            ProjectFilterDto projectFilterDto = projectFilterComponent.getProjectFilterDto();
            projectFilterDto.setPage(query.getPage());
            projectFilterDto.setSize(query.getPageSize());
            projectFilterDto.setOrderList(orderList);
            return projectService.findProjectDtoPage(projectFilterDto);
        };
    }

    private DeleteAction getDeleteAction() {
        return id -> {
            try {
                projectService.deleteProject(id);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex.getMessage(), appEnv);
            }
        };
    }

    @Override
    public void setParameter(BeforeEvent beforeEvent, @OptionalParameter String params) {
        initView(params);
    }
}
