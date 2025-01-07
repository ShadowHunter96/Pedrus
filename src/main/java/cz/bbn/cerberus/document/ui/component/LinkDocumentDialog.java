package cz.bbn.cerberus.document.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.html.H3;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import cz.bbn.cerberus.commons.CssVariables;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.domain.ItemDto;
import cz.bbn.cerberus.commons.component.ui.interfaces.ListAction;
import cz.bbn.cerberus.document.DocumentObjectEnum;
import cz.bbn.cerberus.document.interfaces.LinkAction;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

@Slf4j
public class LinkDocumentDialog extends AppDialog {

    private ComboBox<DocumentObjectEnum> documentEntityEnum;
    private ComboBox<ItemDto> itemList;

    private final ListAction<ItemDto> listAction;
    private final LinkAction linkAction;
    private final String documentId;

    public LinkDocumentDialog(ListAction<ItemDto> listAction, LinkAction linkAction, String documentId) {
        this.listAction = listAction;
        this.linkAction = linkAction;
        this.documentId = documentId;
        initComponent();
    }

    public void initComponent() {
        setTitle(Transl.get("Link document"));
        VerticalLayout verticalLayout = new VerticalLayout();
        verticalLayout.add(new H3(Transl.get("Choose object for link")));

        documentEntityEnum = new ComboBox<>(Transl.get("Object type"));
        List<DocumentObjectEnum> documentObjectEnumList =
                new ArrayList<>(Arrays.stream(DocumentObjectEnum.values()).toList());
        documentObjectEnumList.remove(DocumentObjectEnum.ALL);
        documentEntityEnum.setItems(documentObjectEnumList);
        documentEntityEnum.setItemLabelGenerator(entityEnum -> Transl.get(entityEnum.name()));
        documentEntityEnum.addValueChangeListener(event -> {
            itemList.setValue(null);
            itemList.setItems(listAction.getList(event.getValue().name()));
        });
        verticalLayout.add(documentEntityEnum);

        itemList = new ComboBox<>(Transl.get("Search "));
        itemList.setItemLabelGenerator(itemDto ->
                itemDto.getName() == null ? itemDto.getId() : itemDto.getName()
        );
        itemList.setMinWidth(CssVariables.COMBOBOX_LARGE_WIDTH.getValue());

        verticalLayout.add(itemList);


        setContent(verticalLayout);

        Button link = VaadinComponents.getLinkButton(Transl.get("Link document"));
        link.setDisableOnClick(true);
        link.addClickListener(buttonClickEvent -> {
            linkAction.link(documentEntityEnum.getValue().name(), documentId, itemList.getValue().getId());
            link.setEnabled(true);
            this.close();
        });

        Button close = VaadinComponents.getCloseButton();
        close.addClickListener(buttonClickEvent -> this.close());
        addButtons(link, close);
    }
}
