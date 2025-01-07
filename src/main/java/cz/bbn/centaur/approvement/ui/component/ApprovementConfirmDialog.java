package cz.bbn.cerberus.approvement.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.html.H4;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.data.binder.Binder;
import cz.bbn.cerberus.approvement.ApprovementComponentOperation;
import cz.bbn.cerberus.approvement.dto.ApprovementDto;
import cz.bbn.cerberus.approvement.dto.ApprovementSimpleDto;
import cz.bbn.cerberus.approvement.enums.ApprovementType;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.CountActionDouble;
import cz.bbn.cerberus.translation.Transl;

public class ApprovementConfirmDialog extends AppDialog {

    private final ApprovementComponentOperation approvementComponentOperation;
    private final ApprovementType approvementType;
    private final AppDialog appDialog;
    private final boolean master;
    private final ApprovementDto dto;
    private final Binder<ApprovementDto> binder;
    private final ApprovementDetailComponent approvementDetailComponent;
    private final AppInfiniteGrid<ApprovementSimpleDto> grid;
    private final CountActionDouble countActionDouble;

    public ApprovementConfirmDialog(ApprovementComponentOperation approvementComponentOperation,
                                    ApprovementType approvementType, AppDialog appDialog,
                                    boolean master, ApprovementDto dto,
                                    Binder<ApprovementDto> binder, ApprovementDetailComponent approvementDetailComponent,
                                    AppInfiniteGrid<ApprovementSimpleDto> grid, CountActionDouble countActionDouble) {
        this.approvementComponentOperation = approvementComponentOperation;
        this.approvementType = approvementType;
        this.appDialog = appDialog;
        this.master = master;
        this.dto = dto;
        this.binder = binder;
        this.approvementDetailComponent = approvementDetailComponent;
        this.grid = grid;
        this.countActionDouble = countActionDouble;
        initDialog();
    }

    private void initDialog(){
        setTitle(Transl.get("Another request already exists"));

        H4 label = new H4(Transl.get("In this period already exist another request with type - ").concat(Transl.get(approvementType.name())));

        setContent(label);

        addCloseButton();

        Button submitButton = VaadinComponents.getSubmitButton();

        submitButton.addClickListener(e -> {
            if (binder.validate().isOk()) {
                approvementComponentOperation.getSaveAction(
                        appDialog, master, false, binder,
                        approvementDetailComponent, grid, countActionDouble).saveItem(dto, null);
                grid.loadData();
                if (countActionDouble != null) {
                    countActionDouble.getCount();
                }
                this.close();
            }
        });

        addButtons(submitButton);
    }
}
