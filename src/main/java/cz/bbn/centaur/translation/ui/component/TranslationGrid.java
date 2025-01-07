package cz.bbn.cerberus.translation.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.grid.ColumnTextAlign;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import cz.bbn.cerberus.commons.VaadinValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.ConfirmDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.translation.TranslationComponentOperation;
import cz.bbn.cerberus.translation.dto.TranslationDto;

public class TranslationGrid extends AppInfiniteGrid<TranslationDto> {

    private final TranslationComponentOperation componentOperation;
    private final AppEnv appEnv;

    public TranslationGrid(DeleteAction deleteAction, AppEnv appEnv, ItemsAction<TranslationDto> itemsAction,
                           TranslationComponentOperation componentOperation) {
        super(deleteAction, appEnv, itemsAction);
        this.componentOperation = componentOperation;
        this.appEnv = appEnv;
        initGrid();
    }

    private void initGrid() {
        setSizeFull();
        addColumn(TranslationDto::getId).setHeader(Transl.get("Id")).setSortable(true).setKey("id");
        addColumn(TranslationDto::getLang).setHeader(Transl.get("Language")).setSortable(true).setKey("lang");
        addColumn(TranslationDto::getKey).setHeader(Transl.get("Key")).setSortable(true).setKey("key");
        addColumn(TranslationDto::getValue).setHeader(Transl.get("Value")).setSortable(true).setKey("value");
        addColumn(new ComponentRenderer<>(this::getGridButtons))
                .setHeader(VaadinComponents.headerSpan(Transl.get("Actions")))
                .setWidth(VaadinValues.COLUMN_ACTION_SIZE_SMALL)
                .setFlexGrow(0).setTextAlign(ColumnTextAlign.CENTER);

        addItemDoubleClickListener(e -> new TranslationDialog(
                componentOperation.getCsDto(e.getItem()), componentOperation.getEnDto(e.getItem()),
                componentOperation, this, appEnv).open());
    }


    private HorizontalLayout getGridButtons(TranslationDto dto) {
        HorizontalLayout layout = new HorizontalLayout();
        layout.setHeight("44px");
        Button delete = VaadinComponents.getDeleteButton();
        delete.addClickListener(e -> {
            ConfirmDialog confirmDialog = new ConfirmDialog(
                    Transl.get("Delete translation {0}?", dto.getKey() + " " + dto.getLang()),
                    componentOperation.getDeleteConfirmAction(dto, this));
            confirmDialog.open();
        });
        layout.add(delete);
        return layout;
    }
}
