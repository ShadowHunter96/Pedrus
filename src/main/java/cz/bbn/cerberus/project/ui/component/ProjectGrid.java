package cz.bbn.cerberus.project.ui.component;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.grid.ColumnTextAlign;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import cz.bbn.cerberus.commons.AppGridDataVariables;
import cz.bbn.cerberus.commons.AppGridStringVariables;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.VaadinValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.project.dto.ProjectSimpleDto;
import cz.bbn.cerberus.project.ui.ProjectDetailView;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.translation.Transl;

import java.util.Map;

public class ProjectGrid extends AppInfiniteGrid<ProjectSimpleDto> {

    private final boolean showActions;
    private final Map<String, SubjectDto> subjectMap;
    private final AppEnv appEnv;

    public ProjectGrid(DeleteAction deleteAction, AppEnv appEnv,
                       ItemsAction<ProjectSimpleDto> itemsAction, Map<String, SubjectDto> subjectMap) {
        super(deleteAction, appEnv, itemsAction);
        this.subjectMap = subjectMap;
        this.showActions = true;
        this.appEnv = appEnv;
        initGrid();
    }

    public ProjectGrid(ItemsAction<ProjectSimpleDto> itemsAction, Map<String, SubjectDto> subjectMap,
                       AppEnv appEnv) {
        super(appEnv, itemsAction);
        this.subjectMap = subjectMap;
        this.showActions = false;
        this.appEnv = appEnv;
        initGrid();
    }

    private void initGrid() {
        addColumn(ProjectSimpleDto::getId).setHeader(Transl.get("Id")).setSortable(true).setKey("id");
        addColumn(ProjectSimpleDto::getName).setHeader(Transl.get("Name")).setSortable(true).setKey("name");
        addColumn(new ComponentRenderer<>(this::subjectColumnRenderer)).setHeader(Transl.get("Subject"))
                .setSortable(true).setKey("subject");
        addColumn(new ComponentRenderer<>(this::getUserName)).setHeader(Transl.get("Owner"))
                .setSortable(true).setKey("userId");
        setColumnReorderingAllowed(true);
        setMultiSort(true);

        addColumn(new ComponentRenderer<>(this::getGridButtons))
                .setHeader(VaadinComponents.headerSpan(Transl.get("Actions")))
                .setWidth(VaadinValues.COLUMN_ACTION_SIZE_MEDIUM)
                .setFlexGrow(0).setTextAlign(ColumnTextAlign.CENTER);

        addItemDoubleClickListener(event -> gridClicked(event.getItem().getId()));
    }

    private Span getUserName(ProjectSimpleDto dto) {
        if (dto.getUserDto() != null) {
            if (dto.getUserDto().getAcronym() != null && !dto.getUserDto().getAcronym().isEmpty()) {
                return new Span(dto.getUserDto().getAcronym());
            } else {
                return new Span(dto.getUserDto().getName());
            }
        }
        return new Span();
    }

    private HorizontalLayout getGridButtons(ProjectSimpleDto clickedItem) {
        if (showActions) {
            AppGridStringVariables stringVariables = new AppGridStringVariables("Edit project",
                    "Are you sure you want to delete project {0} ?", "Delete project");
            AppGridDataVariables dataVariables = new AppGridDataVariables(
                    Permission.PROJECT_EDIT, Permission.PROJECT_DELETE,
                    clickedItem.getId(), clickedItem.getName(), ProjectDetailView.ROUTE,
                    DomainEnum.PROJECT_DOMAIN_NAME.getValue(), clickedItem.getDeleted());
            return getSimpleGridButtons(dataVariables, stringVariables, this);
        } else {
            HorizontalLayout buttons = new HorizontalLayout();
            buttons.setClassName("buttons-layout");
            return buttons;
        }
    }

    private Span subjectColumnRenderer(ProjectSimpleDto clickedItem) {
        if (subjectMap.containsKey(clickedItem.getSubject())) {
            return new Span(subjectMap.get(clickedItem.getSubject()).getName());
        }
        return new Span();
    }

    private void gridClicked(String code) {
        if (SecurityUtils.hasCustomPermission(DomainEnum.PROJECT_DOMAIN_NAME.getValue(), code,
                Permission.PROJECT_VIEW.name())) {
            UI.getCurrent().access(() -> UI.getCurrent().navigate(ProjectDetailView.ROUTE + "/" + code));
        } else {
            ErrorNotification.show(Transl.get(ErrorCode.VIEW_PERMISSION_MISSING.getError()), appEnv);
        }
    }
}
