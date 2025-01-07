package cz.bbn.cerberus.labelsubject.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.grid.ColumnTextAlign;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.VaadinValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.DeleteConfirmDialog;
import cz.bbn.cerberus.commons.component.ui.domain.ItemDto;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.label.dto.LabelType;
import cz.bbn.cerberus.labelsubject.dto.LabelSubjectDto;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.List;


public class LabelSubjectGridComponent extends AppInfiniteGrid<LabelSubjectDto> {

    public LabelSubjectGridComponent(DeleteAction deleteAction, AppEnv appEnv,
                                     ItemsAction<LabelSubjectDto> itemsAction) {
        super(deleteAction, appEnv, itemsAction);
        initGrid();
    }

    private void initGrid() {
        addColumn(labelSubjectDto -> labelSubjectDto.getLabelDto().getName());
        addColumn(new ComponentRenderer<>(this::getValueColumn));
        addColumn(new ComponentRenderer<>(this::getGridButtons))
                .setWidth(VaadinValues.COLUMN_ACTION_SIZE_SMALL)
                .setFlexGrow(0).setTextAlign(ColumnTextAlign.CENTER);

        setItemDetailsRenderer(createPersonDetailsRenderer());
        setDetailsVisibleOnClick(false);
        addItemClickListener(event -> {
            if (event.getItem().getLabelDto().getType() == LabelType.TABLE) {
                this.setDetailsVisible(event.getItem(), !this.isDetailsVisible(event.getItem()));
            }
        });
        this.setWidth("35em");
        this.setHeightFull();
    }

    private HorizontalLayout getValueColumn(LabelSubjectDto item) {
        HorizontalLayout horizontalLayout = new HorizontalLayout();
        horizontalLayout.add(getLabel(item));
        return horizontalLayout;
    }

    private static Span getLabel(LabelSubjectDto labelSubjectDto) {
        return switch (labelSubjectDto.getLabelDto().getType()) {
            case TABLE -> getTableStringNames(labelSubjectDto);
            case STRING -> new Span(labelSubjectDto.getText());
            case DATE -> new Span(AppUtils.formatDate(labelSubjectDto.getDate()));
            case NUMBER -> new Span(AppUtils.priceInteger(labelSubjectDto.getInteger()));
            default -> new Span();
        };
    }

    private static ComponentRenderer<LabelSubjectRowDetail, LabelSubjectDto> createPersonDetailsRenderer() {
        return new ComponentRenderer<>(LabelSubjectRowDetail::new,
                (labelSubjectRowDetail, labelCustomerDto) -> labelSubjectRowDetail.setRow(labelCustomerDto));
    }

    private HorizontalLayout getGridButtons(LabelSubjectDto clickedItem) {
        HorizontalLayout buttons = new HorizontalLayout();
        buttons.setClassName("buttons-layout");
        if (SecurityUtils.hasPermission(Permission.LABEL_SUBJECT_DELETE)) {
            Button delete = VaadinComponents.getDeleteButton();
            AppUtils.addRfClassToGridButton(delete, String.valueOf(clickedItem.getId()));
            delete.addClickListener(buttonClickEvent -> {
                DeleteConfirmDialog deleteConfirmDialog =
                        new DeleteConfirmDialog(this, String.valueOf(clickedItem.getId()),
                                Transl.get("Are you sure you want to delete label ?",
                                        String.valueOf(clickedItem.getId())), getAppEnv(), true);
                deleteConfirmDialog.open();
            });
            delete.getElement().setProperty(TextValues.TITLE, Transl.get("Delete label subject"));
            buttons.add(delete);
        }
        return buttons;

    }

    private static Span getTableStringNames(LabelSubjectDto labelSubjectDto) {
        List<String> labelList = new ArrayList<>();
        for (ItemDto label : labelSubjectDto.getTableSet()) {
            labelList.add(label.getName());
        }
        Span tableStringSpan = new Span(StringUtils.join(labelList, ", "));
        tableStringSpan.getElement().getStyle().set("overflow", "hidden").set("white-space", "nowrap")
                .set("text-overflow", "ellipsis");
        tableStringSpan.getElement().setProperty(TextValues.TITLE, StringUtils.join(labelList, ", "));
        tableStringSpan.addClassName("cursor-pointer");
        return tableStringSpan;
    }
}
