package cz.bbn.cerberus.phase.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.binder.Binder;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.phase.PhaseComponentOperation;
import cz.bbn.cerberus.phase.dto.PhaseDto;
import cz.bbn.cerberus.translation.Transl;
import org.apache.commons.lang3.SerializationUtils;

public class PhaseNewDialog extends AppDialog {

    private final PhaseDto phaseDto;
    private final PhaseComponentOperation phaseComponentOperation;
    private final AppInfiniteGrid<PhaseDto> grid;
    private final AppEnv appEnv;
    private final boolean readOnly;

    private final PhaseDto originalDto;

    public PhaseNewDialog(PhaseDto phaseDto, PhaseComponentOperation phaseComponentOperation,
                          AppInfiniteGrid<PhaseDto> grid, AppEnv appEnv, boolean readOnly) {
        this.phaseDto = phaseDto;
        this.phaseComponentOperation = phaseComponentOperation;
        this.grid = grid;
        originalDto = SerializationUtils.clone(phaseDto);
        this.appEnv = appEnv;
        this.readOnly = readOnly;
        initGrid();
    }

    private void initGrid() {
        setTitle(Transl.get("Add phase"));
        setMinHeight("15%");

        VerticalLayout verticalLayout = new VerticalLayout();
        verticalLayout.setMinHeight("7em");
        Binder<PhaseDto> binder = new Binder<>();
        TextField name = new TextField(Transl.get("name"));
        name.setMinWidth("30em");
        name.setMaxLength(100);
        binder.forField(name)
                .asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(PhaseDto::getName, PhaseDto::setName);
        verticalLayout.add(name);
        setContent(verticalLayout);

        binder.setBean(phaseDto);

        Button submit = VaadinComponents.getSubmitButton();
        submit.addClickListener(buttonClickEvent -> {
            if (binder.validate().isOk()) {
                phaseComponentOperation.getSaveAction().saveItem(phaseDto, originalDto);
                grid.loadData();
                this.showWarning(false);
                this.close();
            } else {
                ErrorNotification.show(Transl.get(TextValues.INVALIS_INPUT), appEnv);
            }
        });

        addCloseButton();

        if (readOnly) {
            name.setReadOnly(true);
        } else {
            addSubmitButton(submit);
        }
    }
}
