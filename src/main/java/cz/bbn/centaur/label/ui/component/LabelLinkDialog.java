package cz.bbn.cerberus.label.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.datepicker.DatePicker;
import com.vaadin.flow.component.grid.Grid;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.IntegerField;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.binder.Binder;
import com.vaadin.flow.data.provider.ListDataProvider;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.domain.ItemDto;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.label.LabelComponentOperation;
import cz.bbn.cerberus.label.dto.LabelDto;
import cz.bbn.cerberus.label.dto.LabelType;
import cz.bbn.cerberus.labelsubject.LabelSubjectComponentOperation;
import cz.bbn.cerberus.labelsubject.dto.LabelSubjectDto;
import cz.bbn.cerberus.labelsubject.factory.LabelSubjectFactory;
import cz.bbn.cerberus.translation.Transl;

import java.util.HashSet;

public class LabelLinkDialog extends AppDialog {

    private final AppEnv appEnv;
    private final LabelComponentOperation labelComponentOperation;
    private final String subjectId;
    private final AppInfiniteGrid<LabelSubjectDto> labelSubjectGrid;
    private final LabelSubjectComponentOperation labelSubjectComponentOperation;
    private final LabelSubjectDto labelSubjectDto = new LabelSubjectDto();

    private Binder<LabelSubjectDto> binder;
    private LabelDto selectedLabel;

    public LabelLinkDialog(AppEnv appEnv, LabelComponentOperation labelComponentOperation, String subjectId,
                           AppInfiniteGrid<LabelSubjectDto> labelSubjectGrid,
                           LabelSubjectComponentOperation labelSubjectComponentOperation) {
        this.appEnv = appEnv;
        this.labelComponentOperation = labelComponentOperation;
        this.subjectId = subjectId;
        this.labelSubjectGrid = labelSubjectGrid;
        this.labelSubjectComponentOperation = labelSubjectComponentOperation;
        initComponent();
    }

    private void initComponent() {
        setTitle(Transl.get("Link label"));
        VerticalLayout mainLayout = new VerticalLayout();
        mainLayout.setPadding(false);
        mainLayout.setMargin(false);

        VerticalLayout tableLayout = new VerticalLayout();
        tableLayout.setMargin(false);
        tableLayout.setPadding(false);

        Button search = VaadinComponents.getSearchButton();
        LabelFilterComponent filterComponent = new LabelFilterComponent(search);
        mainLayout.add(filterComponent);

        LabelGridComponent labelGridComponent =
                new LabelGridComponent(appEnv, labelComponentOperation.getItemsAction(filterComponent));
        labelGridComponent.loadData();
        labelGridComponent.setWidthFull();
        tableLayout.add(labelGridComponent);
        search.addClickListener(buttonClickEvent -> labelGridComponent.loadData());

        VerticalLayout valueLayout = new VerticalLayout();
        valueLayout.setMargin(false);
        valueLayout.setPadding(false);
        labelGridComponent.addItemClickListener(labelDtoItemClickEvent -> {
            if (selectedLabel != null && labelDtoItemClickEvent.getItem().equals(selectedLabel)) {
                selectedLabel = null;
                return;
            }
            selectedLabel = labelDtoItemClickEvent.getItem();
            generateLayoutByType(selectedLabel, valueLayout);
        });
        labelGridComponent.setMaxHeight("20em");
        tableLayout.add(valueLayout);
        Button submit = VaadinComponents.getSubmitButton();
        submit.setDisableOnClick(true);
        submit.addClickListener(buttonClickEvent -> {
            if (selectedLabel == null) {
                ErrorNotification.show(Transl.get("You have to choose a label"), appEnv);
            } else if (selectedLabel.getType() == LabelType.TABLE && labelSubjectDto.getTableSet().isEmpty()) {
                ErrorNotification.show(Transl.get("You have to select a value from table"), appEnv);
            } else {
                if (binder.validate().isOk()) {
                    LabelSubjectFactory.fillDto(labelSubjectDto, selectedLabel, subjectId);
                    labelSubjectComponentOperation.getSaveAction(this, labelSubjectGrid, true)
                            .saveItem(labelSubjectDto, null);
                }
            }
            submit.setEnabled(true);
        });
        valueLayout.setWidthFull();
        mainLayout.add(tableLayout);
        this.setContent(mainLayout);
        addCloseButton();
        addButtons(submit);
    }

    public void generateLayoutByType(LabelDto labelDto, VerticalLayout layout) {
        layout.removeAll();
        binder = new Binder<>();
        switch (labelDto.getType()) {
            case DATE:
                DatePicker date = VaadinComponents.getDatePicker(Transl.get("Date"), null);
                date.setWidthFull();
                binder.forField(date).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                        .bind(LabelSubjectDto::getDate, LabelSubjectDto::setDate);
                layout.add(date);
                break;
            case NUMBER:
                IntegerField integerField = new IntegerField(Transl.get("Number"));
                integerField.setWidthFull();
                binder.forField(integerField).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                        .bind(LabelSubjectDto::getInteger, LabelSubjectDto::setInteger);
                layout.add(integerField);
                break;
            case TABLE:
                labelSubjectDto.setTableSet(new HashSet<>());
                layout.add(getGrid(labelDto));
                break;
            case STRING:
                TextField text = new TextField(Transl.get("String"));
                text.setWidthFull();
                binder.forField(text).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                        .bind(LabelSubjectDto::getText, LabelSubjectDto::setText);
                layout.add(text);
                break;
        }

        binder.setBean(labelSubjectDto);
    }

    private Grid<ItemDto> getGrid(LabelDto labelDto) {
        ListDataProvider<ItemDto> dataProvider = new ListDataProvider<>(labelDto.getTableValueList());
        Grid<ItemDto> grid = new Grid<>();
        grid.setWidthFull();
        grid.setMaxHeight("15em");
        grid.addColumn(ItemDto::getName).setHeader(Transl.get("Values"));
        grid.setDataProvider(dataProvider);
        grid.setSelectionMode(Grid.SelectionMode.MULTI);
        grid.getElement().executeJs(
                "this.getElementsByTagName(\"vaadin-grid-flow-selection-column\")[0].hidden = true;");
        grid.addItemClickListener(event -> selectRowAction(event.getItem(), grid));
        return grid;
    }

    private void selectRowAction(ItemDto dto, Grid<ItemDto> grid) {
        if (grid.getSelectedItems().contains(dto)) {
            grid.deselect(dto);
            labelSubjectDto.getTableSet().remove(dto);
        } else {
            grid.select(dto);
            labelSubjectDto.getTableSet().add(dto);
        }
    }

}
