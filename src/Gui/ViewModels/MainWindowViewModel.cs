using System.IO;
using Avalonia.Controls;
using Gui.Views;

namespace Gui.ViewModels
{

    public class MainWindowViewModel : ViewModelBase
    {
        public async void Open()
        {
            var dialog = new OpenFileDialog
            {
                InitialFileName = string.Empty
            };
            dialog.Filters.Add(new FileDialogFilter { Extensions = { "txt" } });
            var result = await dialog.ShowAsync(MainWindow.main);
            if (result is {Length: > 0})
            {
                MainWindow._codeInput.Text = await File.ReadAllTextAsync(result[0]);
            }

            MainWindow.filename = result[0]; }
        
        public async void Save()
        {
            if (!string.IsNullOrEmpty(MainWindow.filename))
            {
                await File.WriteAllTextAsync(MainWindow.filename, MainWindow._codeInput.Text);
            }
            else
            {
                var dialog = new SaveFileDialog
                {
                    InitialFileName = MainWindow.filename
                };
                var result = await dialog.ShowAsync(MainWindow.main);
                if (result != null)
                {
                    await File.WriteAllTextAsync(result, MainWindow._codeInput.Text);
                }
            }

            MainWindow._statusBar.Text = "Changes saved to " + Path.GetFileName(MainWindow.filename);    
            
        }
        
        public async void New()
        {
            var dialog = new SaveFileDialog();
            var result = await dialog.ShowAsync(MainWindow.main);
            if (result != null)
            {
                await File.WriteAllTextAsync(result, MainWindow._codeInput.Text);
            }
            MainWindow.filename = result;
        }
        
    }
}
