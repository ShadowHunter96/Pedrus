package cz.bbn.cerberus.document.ui.component;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.document.DocumentComponentOperation;

import java.io.InputStream;

public class DocumentPreviewDialog extends AppDialog {

    public DocumentPreviewDialog(String documentId, DocumentComponentOperation documentComponentOperation,
                                 DocumentFileTypeEnum type) {
        DocumentViewer documentViewer = new DocumentViewer(documentId, documentComponentOperation, type);
        this.setTitle(documentId);
        this.setContent(documentViewer);
        this.open();

        initComponent();
    }

    public DocumentPreviewDialog(String documentId, InputStream inputStream) {
        DocumentViewer documentViewer = new DocumentViewer(documentId, inputStream);
        this.setTitle(documentId);
        this.setContent(documentViewer);
        this.open();

        initComponent();
    }

    private void initComponent(){
        Button closeButton = VaadinComponents.getCloseButton();
        closeButton.addClickListener(e -> this.close());
        this.addButtons(closeButton);
    }
}
