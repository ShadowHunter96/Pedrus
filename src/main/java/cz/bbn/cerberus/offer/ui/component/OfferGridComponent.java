package cz.bbn.cerberus.offer.ui.component;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.grid.ColumnTextAlign;
import com.vaadin.flow.component.grid.GridSortOrder;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.data.provider.SortDirection;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import cz.bbn.cerberus.commons.AppGridDataVariables;
import cz.bbn.cerberus.commons.AppGridStringVariables;
import cz.bbn.cerberus.commons.AppUtils;
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
import cz.bbn.cerberus.offer.dto.OfferDto;
import cz.bbn.cerberus.offer.ui.OfferDetailView;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;

import java.util.ArrayList;
import java.util.List;

public class OfferGridComponent extends AppInfiniteGrid<OfferDto> {

    private final int showType;
    private final AppEnv appEnv;
    private Column priceWithoutVat;

    public OfferGridComponent(DeleteAction deleteAction, AppEnv appEnv,
                              ItemsAction<OfferDto> itemsAction, int showType) {
        super(deleteAction, appEnv, itemsAction);
        this.showType = showType;
        this.appEnv = appEnv;
        initGrid();
    }

    private void initGrid() {
        this.setSizeFull();
        addColumn(OfferDto::getId).setHeader(Transl.get("Id")).setSortable(true).setKey("id");
        Column<OfferDto> createdColumn = addColumn(offerDto -> AppUtils.formatDateTime(offerDto.getLastUpdate(), true))
                .setSortable(true).setKey("lastUpdate");
        createdColumn.setVisible(false);
        addColumn(OfferDto::getName).setHeader(Transl.get("Name")).setSortable(true).setKey("name");

        switch (showType) {
            case 0:
                addColumn(offerDto -> AppUtils.formatDate(offerDto.getOfferDate())).setHeader(Transl.get("Offer date"))
                        .setSortable(true).setKey("offerDate");
                break;
            case 1:
                addColumn(offerDto -> offerDto.getOpportunityDto().getName())
                        .setHeader(Transl.get("Opportunity")).setSortable(true).setKey("opportunityEntity.name");
                addColumn(offerDto -> Transl.get(offerDto.getState().name()))
                        .setHeader(Transl.get("State")).setSortable(true).setKey("state");
                break;
            case 2:
                addColumn(offerDto -> offerDto.getSubjectDto() != null ? offerDto.getSubjectDto().getName() : "")
                        .setHeader(Transl.get("Customer")).setSortable(true).setKey("subjectEntity.name");
                addColumn(offerDto -> Transl.get(offerDto.getState().name()))
                        .setHeader(Transl.get("State")).setSortable(true).setKey("state");
                break;
            default:
                break;
        }

        addColumn(new ComponentRenderer<>(this::getUserName)).setHeader(Transl.get("Owner"))
                .setSortable(true).setKey("processedByUserEntity");

        priceWithoutVat =  addColumn(offerDto -> AppUtils.priceWithDecimal(offerDto.getPriceWithoutVat()))
                .setHeader(Transl.get("Price without VAT")).setSortable(true).setKey("priceWithoutVat");
        priceWithoutVat.setVisible(false);

        addColumn(new ComponentRenderer<>(this::getGridButtons))
                .setHeader(VaadinComponents.headerSpan(Transl.get("Actions")))
                .setWidth(VaadinValues.COLUMN_ACTION_SIZE_MEDIUM)
                .setFlexGrow(0).setTextAlign(ColumnTextAlign.CENTER);

        addItemDoubleClickListener(event -> gridClicked(event.getItem().getId()));

        GridSortOrder<OfferDto> sortOrder = new GridSortOrder<>(createdColumn, SortDirection.DESCENDING);
        List<GridSortOrder<OfferDto>> sortList = new ArrayList<>();
        sortList.add(sortOrder);
        sort(sortList);

    }

    private HorizontalLayout getGridButtons(OfferDto clickedItem) {
        AppGridStringVariables stringVariables = new AppGridStringVariables("Edit offer",
                "Are you sure you want to delete offer {0} ?", "Delete offer");
        AppGridDataVariables dataVariables = new AppGridDataVariables(
                Permission.OFFER_EDIT, Permission.OFFER_DELETE, clickedItem.getId().replace("/", "&ndash"),
                clickedItem.getName(), OfferDetailView.ROUTE, DomainEnum.OFFER_DOMAIN_NAME.getValue(),
                clickedItem.getDeleted());
        return getSimpleGridButtons(dataVariables, stringVariables, this);
    }

    private void gridClicked(String code) {

        if (SecurityUtils.hasCustomPermission(DomainEnum.OFFER_DOMAIN_NAME.getValue(), code,
                Permission.OFFER_VIEW.name())) {
            UI.getCurrent().navigate(OfferDetailView.ROUTE + "/" + code.replace("/", "&ndash"));
        } else {
            ErrorNotification.show(Transl.get(ErrorCode.VIEW_PERMISSION_MISSING.getError()), appEnv);
        }
    }

    private Span getUserName(OfferDto dto) {
        if (dto.getProcessedByUserDto() != null) {
            if (dto.getProcessedByUserDto().getAcronym() != null
                    && !dto.getProcessedByUserDto().getAcronym().isEmpty()) {
                return new Span(dto.getProcessedByUserDto().getAcronym());
            } else {
                return new Span(dto.getProcessedByUserDto().getName());
            }
        }
        return new Span();
    }

    public Column getPriceWithoutVat() {
        return priceWithoutVat;
    }
}
