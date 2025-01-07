package cz.bbn.cerberus.opportunity.ui.component;

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
import cz.bbn.cerberus.opportunity.dto.OpportunitySimpleDto;
import cz.bbn.cerberus.opportunity.ui.OpportunityDetailView;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.translation.Transl;

import java.util.Map;

public class OpportunityGridComponent extends AppInfiniteGrid<OpportunitySimpleDto> {

    private final Map<String, SubjectDto> subjectMap;
    private final Map<Long, String> userMap;
    private final AppEnv appEnv;

    public OpportunityGridComponent(DeleteAction deleteAction, AppEnv appEnv,
                                    ItemsAction<OpportunitySimpleDto> itemsAction,
                                    Map<String, SubjectDto> subjectMap, Map<Long, String> userMap) {
        super(deleteAction, appEnv, itemsAction);
        this.subjectMap = subjectMap;
        this.userMap = userMap;
        this.appEnv = appEnv;
        initGrid();
    }

    public OpportunityGridComponent(AppEnv appEnv, ItemsAction<OpportunitySimpleDto> itemsAction,
                                    Map<String, SubjectDto> subjectMap, Map<Long, String> userMap) {
        super(appEnv, itemsAction);
        this.subjectMap = subjectMap;
        this.userMap = userMap;
        this.appEnv = appEnv;
        initGrid();
    }

    private void initGrid() {
        setSizeFull();
        addColumn(OpportunitySimpleDto::getId).setHeader(Transl.get("Id")).setSortable(true).setKey("id");
        addColumn(OpportunitySimpleDto::getName).setHeader(Transl.get("Name")).setSortable(true).setKey("name");
        addColumn(new ComponentRenderer<>(this::subjectColumnRenderer))
                .setHeader(Transl.get("Customer"))
                .setSortable(true)
                .setKey("subject");
        addColumn(opportunitySimpleDto -> Transl.get(opportunitySimpleDto.getState().name()))
                .setHeader(Transl.get("State")).setSortable(true).setKey("state");
        addColumn(new ComponentRenderer<>(this::getUserName)).setHeader(Transl.get("Owner"))
                .setSortable(true).setKey("userId");
        addColumn(new ComponentRenderer<>(opportunitySimpleDto ->
                VaadinComponents.getPctColumn(opportunitySimpleDto.getProgress())))
                .setHeader(Transl.get("Progress"))
                .setWidth(VaadinValues.COLUMN_ICON_SIZE_MEDIUM)
                .setFlexGrow(0)
                .setSortable(true)
                .setKey("progress");

        addColumn(new ComponentRenderer<>(this::getGridButtons))
                .setHeader(VaadinComponents.headerSpan(Transl.get("Actions")))
                .setWidth(VaadinValues.COLUMN_ACTION_SIZE_MEDIUM)
                .setFlexGrow(0).setTextAlign(ColumnTextAlign.CENTER);

        setColumnReorderingAllowed(true);
        setMultiSort(true);

        addItemDoubleClickListener(event -> gridClicked(event.getItem().getId()));
    }

    private HorizontalLayout getGridButtons(OpportunitySimpleDto clickedItem) {
        AppGridStringVariables stringVariables = new AppGridStringVariables("Edit opportunity",
                "Are you sure you want to delete opportunity {0} ?", "Delete opportunity");
        AppGridDataVariables dataVariables = new AppGridDataVariables(
                Permission.OPPORTUNITY_EDIT, Permission.OPPORTUNITY_DELETE,
                clickedItem.getId().replace("/", "&ndash"), clickedItem.getName(),
                OpportunityDetailView.ROUTE, DomainEnum.OPPORTUNITY_DOMAIN_NAME.getValue(), clickedItem.getDeleted());
        return getSimpleGridButtons(dataVariables, stringVariables, this);
    }

    private Span subjectColumnRenderer(OpportunitySimpleDto clickedItem) {
        if (subjectMap.containsKey(clickedItem.getSubject())) {
            return new Span(subjectMap.get(clickedItem.getSubject()).getName());
        }
        return new Span();
    }

    private void gridClicked(String code) {
        if (SecurityUtils.hasCustomPermission(DomainEnum.OPPORTUNITY_DOMAIN_NAME.getValue(), code,
                Permission.OPPORTUNITY_VIEW.name())) {
            UI.getCurrent().navigate(OpportunityDetailView.ROUTE + "/" + code.replace("/", "&ndash"));
        } else {
            ErrorNotification.show(Transl.get(ErrorCode.VIEW_PERMISSION_MISSING.getError()), appEnv);
        }
    }

    private Span getUserName(OpportunitySimpleDto dto) {
        if (userMap.containsKey(dto.getUserId())) {
            return new Span(userMap.get(dto.getUserId()));
        }
        return new Span();
    }
}
